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
  putStrLn "Welcome to hassowrd:A deterministc password manager in haskell!"
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
        --mapM_ print [site,user,addition] --debug
        let info = show (Entry site user addition)
        putStrLn $ calculatePassowrd key info
        saveDb (info++"\n")
          
  let showEntry getIdx message = do
        infos <- readDb
        let Right entries = parseConfig  infos
        idx <- getIdx entries
        let entry = entries !! idx 
        putStrLn $ message++show entry
        let pwd = calculatePassowrd key (show entry)
        putStrLn pwd
            
  let doSearch = showEntry
                 (\ entries -> do
                     (>:) "Give me some information:"
                     x <- getLine
                     return $ fuzzSearch x entries)
                 "The nearest entry:"
        
  let doShow = do
        infos <- lines <$> readDb
        mapM_ (\ (row,l) -> putStrLn (show row++" "++l)) (zip [1..] infos)
       
  forever $ do
    (>:) "> "
    cmd <- getLine
    case cmd of
        "record" -> doRecord
        "search" -> doSearch
        "show"   -> doShow
        "exit"   -> exitSuccess
        "help"   -> help
        _        -> if all isDigit cmd then showEntry (\ _ -> return (read cmd :: Int)) mempty
                   else help
     where help = putStrLn "[record|check|show|exit]"
