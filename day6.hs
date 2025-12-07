{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

import Control.Monad (ap)
import Data.Bool (bool)
import Data.Function (on)
import Data.List (transpose)

split :: (a -> Bool) -> [a] -> [[a]]
split = ((map fst . takeWhile (uncurry ((||) `on` not . null))) .) . (ap =<< (((.) . iterate . (. drop 1 . snd)) .)) break

part1 :: String -> [([Int], Int -> Int -> Int)]
part1 = map (((,) <$> map read . init <*> bool (+) (*) . (== "*") . last)) . transpose . map (filter (not . null) . words) . lines

part2 :: String -> [([Int], Int -> Int -> Int)]
part2 = (zip <$> map (map read) . split null . map (takeWhile (/= ' ') . dropWhile (== ' ')) . transpose . init <*> map (bool (+) (*) . (== "*")) . filter (not . null) . words . last) . lines

solve :: [([Int], Int -> Int -> Int)] -> Int
solve = sum . map (uncurry $ flip foldl1)

main :: IO ()
main = do
  input <- readFile "day6.txt"
  print . solve . part1 $ input
  print . solve . part2 $ input
