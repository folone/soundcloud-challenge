{-# LANGUAGE UnicodeSyntax #-}
import           Data.List
import           Util

main :: IO ()
main = interact calculate

calculate :: String → String
calculate = unlines . reduce . lines

reduce :: [String] → [String]
reduce = map (stringify . parse) where
  stringify (y, ys, yys) =
    let lst = ys ++ yys
        sorted = sort . nub $ lst
    in y ++ "\t" ++ intercalate "\t" sorted
