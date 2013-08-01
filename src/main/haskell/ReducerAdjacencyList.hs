{-# LANGUAGE UnicodeSyntax #-}
import           Util

main :: IO ()
main = interact calculate

calculate :: String → String
calculate = unlines . process . lines

process :: [String] → [String]
process = map stringifyPair . customGroupPairs . map parsePair