{-# LANGUAGE UnicodeSyntax #-}
import           Util

main :: IO ()
main = interact calculate

calculate :: String → String
calculate = unlines . process . lines

process :: [String] → [String]
process = map stringify . group . map (parse :: String → (String, [String]))
