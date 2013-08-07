{-# LANGUAGE UnicodeSyntax #-}
import           Util

main :: IO ()
main = interact calculate

calculate :: String → String
calculate = unlines . (>>= process) . lines

process :: String → [String]
process xs =
  let (h, adjLst, t)     = parse xs :: (String, [String], [String])
      myStringify (y:ys) = stringify (y, [h], ys ++ t)
      myStringify []     = "" -- being total
  in map myStringify (cycleThrough adjLst)
