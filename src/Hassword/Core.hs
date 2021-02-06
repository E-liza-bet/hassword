module Hassword.Core where
import System.IO
import Data.Char
import Control.Monad (liftM2)
import Hassword.Algorithm
import Hassword.Crypto
import Hassword.Database
import Hassword.Config

verify :: String -> Bool
verify s = all (`any` s) [isUpper,isDigit,isLower, liftM2 (||) isPunctuation isSymbol]

calculatePassowrd ::  String -> Entry -> String
calculatePassowrd key entry = let
  iterate round = if verify pwd then pwd else iterate (round+1)
    where f = hmac hash (ascii2b key) (ascii2b (show entry))
          part    = b2asciiWith alphabet (f round)
          part'   = take _SpecialCharsNum $ b2asciiWith _Alphabet' (f round)
          pwd = part <>  part'
  in iterate _HmacMinRound
  
entryScore :: String -> Entry -> Int
entryScore s (Entry site user addition) = maximum $ map (similarity s) [site,user,addition]

fuzzSearch :: String -> [Entry] -> (Int,Entry)
fuzzSearch = maxScoreBy entryScore
