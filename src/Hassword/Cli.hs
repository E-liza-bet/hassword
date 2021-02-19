module Hassword.Cli (cli) where
import System.IO
import System.Exit (exitSuccess)
import System.Environment (getEnv)
import System.Clipboard (setClipboardString)
import Control.Exception (bracket_)
import Control.Monad (forever,unless)
import Data.Char(isDigit,ord,chr)
import Data.List(isPrefixOf)

import Hassword.Database
import Hassword.Core

help :: String
help = "Welcome to hassword, a deterministic password manager!\n"++
       "Remember the master key forever or write it down somewhere "++
       "because it is used to derive all "++
       "the other keys.\n"++
       "record :: Fill in some public information to calculate a strong password and send it to clipboard.\n"++
       "search :: Fuzzy search to find the most similar entry, calculate it again.\n"++
       "show   :: Show all the entries with index which can be typed in to get the coresponding password.\n"++
       "edit   :: Edit entries directly. Make sure you know what you are doing!"
       
withEcho echo action = do
        old <- hGetEcho stdin
        bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
        
readline :: String -> [String] -> String -> IO String
readline prompt completions ready=do
  oldEcho <- hGetEcho stdin
  oldIn <- hGetBuffering stdin
  oldOut <- hGetBuffering stdout
  let fin = do
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
  let fout = do
        hSetEcho stdin oldEcho
        hSetBuffering stdin oldIn
        hSetBuffering stdout oldOut
  bracket_ fin fout (readline' prompt completions ready)
  
readline' :: String -> [String] -> String -> IO String
readline' prompt completions ready = do
    c <- getChar
    case c of
        '\t' -> if not (null ready)
               then do
          let lastword = last $ words (reverse ready)
          let possibles = filter (isPrefixOf lastword) completions
          case length possibles of
            0 -> next ready
            1 -> do
              let left = drop (length lastword) (head possibles)
              putStr left
              next $ reverse left ++ ready
            _ -> do
              putStrLn ""
              putStrLn $ unwords possibles
              putStr $ prompt++reverse ready
              next ready
               else do
          putStrLn ""
          putStrLn $ unwords completions
          putStr prompt
          next ready
        '\DEL' -> 
          if not (null ready)
            then do
            putChar '\b'
            putChar ' '
            putChar '\b'
            next (tail ready)
            else next ready
        '\n' -> do
          putChar '\n'
          return $ reverse ready
        _ -> do
            putChar c -- do echo everything else
            next (c:ready)
   where next = readline prompt completions
         
cli :: IO ()
cli = do
  let (>:) s = do putStr s;hFlush stdout
  (>:) "Input master key:"
  key <- withEcho False getLine
  putStrLn ""
  let doRecord = do  
        (>:) "Input site:"
        site <- getLine
        (>:) "Input user:"
        user <- getLine
        (>:) "Input addition: "
        addition <- getLine
        let entry = Entry site user addition
        let pwd = calculatePassowrd key entry
        putStrLn pwd
        setClipboardString pwd
        saveDb entry
          
  let showEntry getIdx message = do
        m <- readDb
        case m of
          Just entries -> do
            idx <- getIdx entries
            if idx < 0 || idx > length entries - 1
              then return ()
              else do let entry = entries !! idx 
                      putStrLn $ message++show entry
                      let pwd = calculatePassowrd key entry
                      setClipboardString pwd
                      putStrLn pwd
          Nothing -> return ()
            
  let doSearch = showEntry
                 (\ entries -> do
                     (>:) "Give me some information:"
                     x <- getLine
                     return $ fst (fuzzSearch x entries))
                 "The nearest entry:"
        
  let doShow = do
        m <- readDb
        case m of
          Just entries -> mapM_ (\ (row,l) ->
                                   putStrLn (show row++" "++show l))
                         (zip [1..] entries)
          Nothing -> return ()

  --hSetEcho stdin False
  --hSetBuffering stdin NoBuffering
  --hSetBuffering stdout NoBuffering
  
  forever $ do
    (>:) "> "
    cmd <- readline "> " ["record","search","show","edit","exit","help"] []
    let cmds = words cmd
    unless (null cmds) $
      case head cmds of
        "record" -> doRecord
        "search" -> doSearch
        "show"   -> doShow
        "edit"   -> editDb
        "exit"   -> exitSuccess
        "help"   -> putStrLn help
        _        -> if all isDigit cmd
                   then showEntry (\ _ -> return ((read cmd :: Int)-1)) mempty
                   else putStrLn "Command:[help|record|search|show|edit|exit]"
