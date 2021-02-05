module Hassword.Cli (cli) where
import System.IO
import System.Exit (exitSuccess)
import System.Environment (getEnv)
import Control.Exception (bracket_)
import Control.Monad (forever)
import Data.Char(isDigit)
import Hassword.Config
import Hassword.Core

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
        putStrLn $ calculatePassowrd key entry
        saveDb entry
          
  let showEntry getIdx message = do
        m <- readDb
        case m of
          Just entries -> do
            idx <- getIdx entries
            let entry = entries !! idx 
            putStrLn $ message++show entry
            let pwd = calculatePassowrd key entry
            putStrLn pwd
          Nothing -> return ()
            
  let doSearch = showEntry
                 (\ entries -> do
                     (>:) "Give me some information:"
                     x <- getLine
                     return $ fuzzSearch x entries)
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
        "exit"   -> exitSuccess
        "help"   -> help
        _        -> if all isDigit cmd
                   then showEntry (\ _ -> return ((read cmd :: Int)-1)) mempty
                   else help
     where help = putStrLn "Command:[help|record|search|show|exit]"
