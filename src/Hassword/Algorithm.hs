module Hassword.Algorithm where
import Data.List (isSubsequenceOf)

subPatternWith :: Int -> [a] -> [[a]]
subPatternWith _ [] = []
subPatternWith 1 (c:cs) = [c] : subPatternWith 1 cs
subPatternWith x (c:cs) = map (c:) ( subPatternWith (x-1) cs) ++
                          subPatternWith x cs

subPattern :: [a] -> [[a]]
subPattern s = foldMap (`subPatternWith` s) [1..length s]

similarity :: Eq a => [a] -> [a] -> Int
similarity x y = maximum [ if any (`isSubsequenceOf` y) (subPatternWith i x)
                           then i else 0  |i <- [1..length x]]

linearSearch valueFunc x ys =
  let f = foldl (\ (ans,idx,score) y ->
                    let score' = valueFunc x y
                    in if score' > score
                       then (idx+1,idx+1,score') else (ans,idx+1,score))
          (-1,-1,0)
      (ans,_,_) = f ys
  in ans


