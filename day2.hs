{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

import Control.Monad (ap)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.Function (on)
import Data.List (findIndex)
import Data.Maybe (fromJust)

-- Haskell has `lines` and `words` but no generic `split` function
split :: (a -> Bool) -> [a] -> [[a]]
split = ((map fst . takeWhile (uncurry ((||) `on` not . null))) .) . (ap =<< (((.) . iterate . (. drop 1 . snd)) .)) break

allSame :: (Eq a) => [a] -> Bool
allSame = bool False <$> ap (all . (==) . head) tail <*> not . null

countDigits :: Int -> Int
countDigits = fromJust . findIndex (== 0) . iterate (`div` 10)

divisors :: Int -> [Int]
divisors = filter (/= 1) . (concatMap <$> (ap (curry $ bool <$> uncurry ((. pure) . (:)) <*> pure . fst <*> uncurry (==)) . div) <*> (filter <$> ((== 0) .) . mod <*> enumFromTo 1 . (min <$> pred <*> ceiling . sqrt . toEnum)))

numberParts :: Int -> Int -> Int -> [Int]
numberParts = (. ((.) =<< iterate . (. fst)) . flip divMod . (10 ^)) . (.) . (map snd .) . take

parse :: String -> [(Int, Int)]
parse = map (bimap read (read . tail) . break (== '-')) . split (== ',')

solve :: (Int -> [Int]) -> [(Int, Int)] -> Int
solve = (sum .) . map . (sum .) . (. uncurry enumFromTo) . filter . (any allSame .) . ap (map . flip (numberParts <$> snd <*> uncurry div)) . (. countDigits) . (zip <$> repeat <*>)

main :: IO ()
main = do
  input <- parse <$> readFile "day2.txt"
  print . solve (bool [] [2] . (== 0) . (`mod` 2)) $ input
  print . solve divisors $ input
