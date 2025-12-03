{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

import Control.Monad (ap, liftM2)
import Data.Maybe (fromMaybe)

parse :: String -> [[Int]]
parse = map (map $ read . pure) . lines

step :: Int -> [Int] -> Maybe [Int] -> [Int]
step = curry . (liftM2 max <$> flip ((++) . fst) . pure <*> (. snd) . (fromMaybe [] .) . (<$>) . ap ((. pure) . (++) . init) . (. last) . max)

solve :: Int -> [[Int]] -> Int
solve = (sum .) . map . ((foldl1 ((+) . (* 10)) . head) .) . (. foldl (ap (flip . flip (zipWith . step) . ([] :)) ((++ [Nothing]) . map Just)) []) . drop

main :: IO ()
main = do
  input <- parse <$> readFile "day3.txt"
  print . solve 1 $ input
  print . solve 11 $ input
