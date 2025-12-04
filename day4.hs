{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

import Control.Monad (ap)
import Data.Bool (bool)
import Data.List (transpose)

parse :: String -> [[Bool]]
parse = map (map (== '@')) . lines

accessible :: [[Bool]] -> [(Int, Int)]
accessible = concat . concat . zipWith (flip zipWith [0 ..] . curry ((. ((&&) <$> head . tail . head . tail <*> (< 5) . length . filter id . concat)) . bool [] . pure)) [0 ..] . ((.) =<< map . (. transpose)) (zipWith3 ((flip (:) .) . flip (:) . pure) <$> ((:) =<< flip take (repeat False) . length . head) <*> id <*> ((++) <$> drop 1 <*> pure . flip take (repeat False) . length . head))

part1 :: [[Bool]] -> Int
part1 = length . accessible

part2 :: [[Bool]] -> Int
part2 = sum . map (length . fst) . takeWhile (not . null . fst) . tail . iterate (ap (ap (,) . (foldr (curry ((flip (++) .) . (:) <$> ((flip (++) . (False :) <$> uncurry (drop . (+ 1)) <*> uncurry take) . ((,) <$> snd . fst <*> head . uncurry (drop . fst))) <*> uncurry (drop . (+ 1) . fst) <*> uncurry (take . fst))) . snd)) $ accessible . snd) . ([],)

main :: IO ()
main = do
  input <- parse <$> readFile "day4.txt"
  print . part1 $ input
  print . part2 $ input
