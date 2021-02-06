module Hassword.Database (Entry(..),parseDb,readDb,saveDb,editDb) where
import Text.ParserCombinators.Parsec
import System.Environment
import System.Process (callCommand)
import Data.Maybe (isJust)
import Data.List (find)
import Control.Exception (catch,IOException)
import Hassword.Config

data Entry = Entry {getSite::String,getUser::String,getAddition::String}

instance Show Entry where
  show (Entry s u a) = s .|. u .|. a
    where x .|. y = x <> "|" <> y

entryParser :: GenParser Char st Entry
entryParser = do
  site <- many (noneOf "|\n")
  char '|'
  user <- many (noneOf "|\n")
  char '|'
  addition <- many (noneOf "|\n")
  many (oneOf " \t\n")
  return $ Entry site user addition

dbParser :: GenParser Char st [Entry]
dbParser = many entryParser

parseDb = parse dbParser "config parse error"

saveDb :: Entry -> IO ()
saveDb e = do
  home <- getEnv "HOME"
  appendFile  (home ++ _DbPath) (show e ++ "\n")
  
readDb :: IO (Maybe [Entry])
readDb = do
  home  <- getEnv "HOME"
  infos <- catch (readFile (home ++ _DbPath))
    (\ e -> do
        let err = show (e :: IOException)
        putStrLn "No database file,record first"
        return "")
  if infos == ""
    then return Nothing
    else case parseDb infos of
           Right entries -> return (Just entries)
           Left  error -> return Nothing
           
editDb :: IO ()
editDb = do
  home  <- getEnv "HOME"
  let editWith c = callCommand $ c ++ " " ++ (home ++ _DbPath)
  editors <- mapM lookupEnv ["EDITOR","VISUAL"]
  case find isJust editors of
    Just (Just editor) -> editWith editor
    Nothing -> editWith _DefaultEditor
 
