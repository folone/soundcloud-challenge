{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}
import           Data.List
import           Data.List.Split

main :: IO ()
main = interact calculate

calculate :: String → String
calculate text = unlines . map stringify . convert . lines $ text where
  stringify = intercalate "\t"

convert :: [String] → [[String]]
convert xs = xs >>= convert' where
  convert' = cycleThrough . splitOn "\t"

cycleThrough :: [a] → [[a]]
cycleThrough ys =
  let size = length ys
      lst  = cycle ys
  in map (\n → take size (drop n lst)) [0 .. size - 1]
