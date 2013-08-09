{-# LANGUAGE UnicodeSyntax #-}
import           Util

main :: IO ()
main = interact calculate

calculate :: String → String
calculate = unlines . reduce . lines

reduce :: [String] → [String]
reduce = map stringify . group . map (parse :: String → (String, [String], [String]))
