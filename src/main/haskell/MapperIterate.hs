{-# LANGUAGE UnicodeSyntax #-}
import           Data.List

import           Util

main :: IO ()
main = interact calculate

calculate :: String → String
calculate = unlines . (>>= process) . lines

process :: String → [String]
process xs =
  let (h, adjLst, t) = parseTriple xs
      stringify (y:ys) = stringifyTriple (y, [h], ys ++ t)
  in map stringify (cycleThrough adjLst)
