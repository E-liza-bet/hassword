import Hassword.Algorithm
import Hassword.Crypto
import Test.QuickCheck
import Data.Word (Word64)

prop_len :: String -> Bool
prop_len s = length (subPattern s) == 2 ^ (length s) - 1

prop_conflict :: [Word64] -> [Word64] -> Bool
prop_conflict x y = if x /= y
                    then hash x /= hash y
                    else hash x == hash y

main :: IO ()
main = do
  quickCheck (withMaxSuccess 20 prop_len)
  quickCheck (withMaxSuccess 500 prop_conflict)
