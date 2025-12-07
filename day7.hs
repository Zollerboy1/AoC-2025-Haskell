{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

import Control.Monad (liftM2)
import Data.Bifunctor (bimap, first)
import Data.Bool (bool)
import Data.Function (on)
import Data.List (find, findIndex, groupBy, nub)
import Data.Maybe (fromJust, fromMaybe)

part1 :: [String] -> Int
part1 = snd . (foldl ((first nub .) . (. zip [0 ..]) . (foldr <$> ((bimap <$> (++) . fst <*> (+) . snd) .) . liftM2 (bool ([], 0)) (bool <$> (,0) . pure . fst <*> (,1) . ([pred, succ] <*>) . pure . fst <*> (== '^') . snd) . ((. fst) . flip elem . fst) <*> ([],) . snd)) <$> (,0) . pure . fromJust . findIndex (== 'S') . head <*> tail)

part2 :: [String] -> Int
part2 = sum . map snd . (foldl (((map (foldr1 (liftM2 (,) fst . ((+) `on` snd))) . groupBy ((==) `on` fst)) .) . (. zip [0 ..]) . flip foldr [] . (((++) . fromMaybe []) .) . liftM2 (<$>) (flip (liftM2 bool pure ((map first [pred, succ] <*>) . pure)) . (== '^') . snd) . ((. (. fst) . (==) . fst) . flip find)) <$> pure . (,1) . fromJust . findIndex (== 'S') . head <*> tail)

main :: IO ()
main = do
  input <- lines <$> readFile "day7.txt"
  print . part1 $ input
  print . part2 $ input
