module Hassword.Config (Entry(..),parseConfig) where
import Text.ParserCombinators.Parsec

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

configParser :: GenParser Char st [Entry]
configParser = many entryParser

parseConfig = parse configParser "config parse error"
