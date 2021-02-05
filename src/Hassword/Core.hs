module Hassword.Core where
import System.IO
import System.Environment
import Data.Char
import Control.Monad (liftM2)
import Hassword.Config
import Hassword.Algorithm
import Hassword.Crypto

verify :: String -> Bool
verify s = all (`any` s) [isUpper,isDigit,isLower, liftM2 (||) isPunctuation isSymbol]

calculatePassowrd ::  String -> String -> String
calculatePassowrd key message = let
  test round = if verify pwd then pwd else test (round+1)
    where f = hmac hash (ascii2b key) (ascii2b message)
          part    = b2asciiWith alphabet (f round)
          part'   = take 4 $ b2asciiWith alphabet' (f round)
          pwd = part <>  part'
  in test 20

dbPath = "/.config/hasword.db" :: String

saveDb :: String -> IO ()
saveDb info = do
  home <- getEnv "HOME"
  appendFile  (home ++ dbPath) info
  
readDb :: IO String
readDb = do
  home <- getEnv "HOME"
  readFile (home++dbPath)

entryScore :: String -> Entry -> Int
entryScore s (Entry site user addition) = maximum $ map (similarity s) [site,user,addition]

fuzzSearch :: String -> [Entry] -> Int
fuzzSearch = linearSearch entryScore
