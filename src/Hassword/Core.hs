module Hassword.Core where
import System.IO
import System.Environment
import System.Process (callCommand)
import System.Exit
import Data.Char
import Data.List (find)
import Data.Maybe (isJust)
import Control.Monad (liftM2)
import Control.Exception (catch,IOException)
import Hassword.Config
import Hassword.Algorithm
import Hassword.Crypto

verify :: String -> Bool
verify s = all (`any` s) [isUpper,isDigit,isLower, liftM2 (||) isPunctuation isSymbol]

calculatePassowrd ::  String -> Entry -> String
calculatePassowrd key entry = let
  test round = if verify pwd then pwd else test (round+1)
    where f = hmac hash (ascii2b key) (ascii2b (show entry))
          part    = b2asciiWith alphabet (f round)
          part'   = take 4 $ b2asciiWith alphabet' (f round)
          pwd = part <>  part'
  in test 20

dbPath = "/.config/hasword.db" :: String

saveDb :: Entry -> IO ()
saveDb e = do
  home <- getEnv "HOME"
  appendFile  (home ++ dbPath) (show e ++ "\n")
  
readDb :: IO (Maybe [Entry])
readDb = do
  home  <- getEnv "HOME"
  infos <- catch (readFile (home++dbPath))
    (\ e -> do
        let err = show (e :: IOException)
        putStrLn "No database file,record first"
        return "")
  if infos == ""
    then return Nothing
    else case parseConfig infos of
           Right entries -> return (Just entries)
           Left  error -> return Nothing
           
editDb :: IO ()
editDb = do
  home  <- getEnv "HOME"
  let editWith c = callCommand $ c ++ " " ++ (home++dbPath)
  editors <- mapM lookupEnv ["EDITOR","VISUAL"]
  let defaultEditor = "vi"
  case find isJust editors of
    Just (Just editor) -> editWith editor
    Nothing -> editWith defaultEditor
  
  
entryScore :: String -> Entry -> Int
entryScore s (Entry site user addition) = maximum $ map (similarity s) [site,user,addition]

fuzzSearch :: String -> [Entry] -> (Int,Entry)
fuzzSearch = maxScoreBy entryScore
