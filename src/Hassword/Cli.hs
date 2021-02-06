module Hassword.Cli (cli) where
import System.IO
import System.Exit (exitSuccess)
import System.Environment (getEnv)
import System.Clipboard (setClipboardString)
import Control.Exception (bracket_)
import Control.Monad (forever)
import Data.Char(isDigit)
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

  forever $ do
    (>:) "> "
    cmd <- getLine
    case cmd of
        "record" -> doRecord
        "search" -> doSearch
        "show"   -> doShow
        "edit"   -> editDb
        "exit"   -> exitSuccess
        "help"   -> putStrLn help
        _        -> if all isDigit cmd
                   then showEntry (\ _ -> return ((read cmd :: Int)-1)) mempty
                   else putStrLn "Command:[help|record|search|show|edit|exit]"
