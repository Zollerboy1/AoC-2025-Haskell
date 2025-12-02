{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

import Control.Monad (ap, join, liftM2)
import Data.Bifunctor (bimap)
import Data.Function (applyWhen, on)

parse :: String -> [Int]
parse = scanl ((. ((`applyWhen` negate) . (== 'L') <$> head <*> read . tail)) . (+)) 50 . lines

part1 :: [Int] -> Int
part1 = sum . map (fromEnum . (== 0) . (`mod` 100))

part2 :: [Int] -> Int
part2 = sum . map (ap (applyWhen <$> uncurry (>) <*> uncurry ((. (,succ)) . on (.) (uncurry applyWhen) . (,pred)) . join bimap ((== 0) . (`mod` 100))) $ uncurry ((abs .) . on (-) (`div` 100))) . ap zip tail

main :: IO ()
main = do
  input <- parse <$> readFile "day1.txt"
  print . part1 $ input
  print . part2 $ input
