{-# LANGUAGE UnicodeSyntax     #-}
import           Data.List
import           Data.List.Split

import Util

main :: IO ()
main = interact calculate

calculate :: String → String
calculate = unlines . map stringify . convert . lines where
  stringify = intercalate "\t"

convert :: [String] → [[String]]
convert xs = xs >>= convert' where
  convert' = cycleThrough . splitOn "\t"
