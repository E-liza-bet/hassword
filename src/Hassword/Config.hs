module Hassword.Config where

_DbPath = "/.config/hasword.db" :: String --under $HOME
_DefaultEditor = "vi" :: String

-- <=8
_SpecialCharsNum :: Int
_SpecialCharsNum = 4
_HmacMinRound = 20 :: Int
_Alphabet' = "!@#$%&*()-,[]{}" :: String

-- special characters

_HashAddition = 0xdeadbeaf
_HashMultiplient = 0xbeebee
