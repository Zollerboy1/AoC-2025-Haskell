{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

import Control.Monad (ap, liftM2)
import Data.Maybe (fromMaybe)

step :: Char -> String -> Maybe String -> String
step = curry . (liftM2 max <$> flip ((++) . fst) . pure <*> (. snd) . (fromMaybe [] .) . (<$>) . ap ((. pure) . (++) . init) . (. last) . max)

solve :: Int -> [String] -> Int
solve = (sum .) . map . ((read . head) .) . (. foldl (ap (flip . flip (zipWith . step) . ([] :)) ((++ [Nothing]) . map Just)) []) . drop

main :: IO ()
main = do
  input <- lines <$> readFile "day3.txt"
  print . solve 1 $ input
  print . solve 11 $ input
