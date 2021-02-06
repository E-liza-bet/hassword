module Hassword.Algorithm where
import Data.List (isSubsequenceOf,maximumBy)

subPatternWith :: Int -> [a] -> [[a]]
subPatternWith _ [] = []
subPatternWith 1 (c:cs) = [c] : subPatternWith 1 cs
subPatternWith x (c:cs) = map (c:) ( subPatternWith (x-1) cs) ++
                          subPatternWith x cs

subPattern :: [a] -> [[a]]
subPattern s = foldMap (`subPatternWith` s) [1..length s]

similarity :: Eq a => [a] -> [a] -> Int
similarity x y = maximum [ if any (`isSubsequenceOf` y) (subPatternWith i x)
                           then i else 0  | i <- [1..length x]]

maxScoreBy valueFunc x ys = select $ maximumBy (\ (i,_,score) (i',_,score') -> score `compare` score')
                            $ zip3 [0..] ys (map (valueFunc x) ys)
                            where select (x,y,_) = (x,y)
