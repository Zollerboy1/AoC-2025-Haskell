{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

import Control.Monad (ap, liftM2, liftM3)
import Data.Bifunctor (bimap, first)
import Data.Bool (bool)
import Data.Function (applyWhen, on)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)

parse :: String -> ([(Int, Int)], [Int])
parse = bimap (map (bimap read ((+ 1) . read . tail) . break (== '-'))) (map read . tail) . break null . lines

part1 :: [(Int, Int)] -> [Int] -> Int
part1 = (length .) . filter . flip (any . (liftM2 . liftM2) (&&) ((. fst) . (>=)) ((. snd) . (<)))

prepend :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
prepend = (uncurry (:) .) . (liftM2 . liftM2) first (const . (liftM3 . liftM3) bool (((liftM2 . liftM2) (,) ((. fst . head) . min . fst) ((. snd . last) . max . snd))) (const) (const null)) (break . (. fst) . (<) . snd)

insert :: Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
insert = liftM2 ap ((liftM2 . liftM2 . liftM2) (++) (const (flip take)) ((. flip drop) . (.) . prepend)) . (flip =<< ((flip . (flip applyWhen (subtract 1) .)) .) . ap ((.) . (.) . (&&) . (> 0)) (flip ((.) . (<=) . fst) . ((snd . last) .) . take))

append :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
append = (liftM2 . liftM2) (++) (const init) ((. last) . ((liftM3 . liftM3) bool (flip (:) . pure) ((pure .) . liftM2 (,) fst . on max snd) ((. snd) . (<=) . fst)))

part2 :: [(Int, Int)] -> Int
part2 = sum . map (uncurry subtract) . (foldr (liftM2 ap ((liftM2 . liftM2 . liftM2) fromMaybe ((const .) . append) ((fmap .) . flip . flip insert)) (findIndex . (. fst) . (<) . fst)) <$> pure . head <*> tail)

main :: IO ()
main = do
  input <- parse <$> readFile "day5.txt"
  print . uncurry part1 $ input
  print . part2 . fst $ input
