{-# LANGUAGE UnicodeSyntax #-}
import Data.List
import Data.List.Split

main :: IO ()
main = interact calculate

calculate :: String → String
calculate text = unlines . process $ lines text
    
process :: [String] → [String]
process xs = map process' xs where
  process' str  =
    let lst     = splitOn "\t" str
        user    = head lst
        friends = sort . nub . filter (\f → f /= user) $ (tail lst)
    in user ++ "\t" ++ intercalate "\t" friends
