{-# LANGUAGE UnicodeSyntax #-}
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Ord

main :: IO ()
main = interact calculate

calculate :: String → String
calculate text = unlines . process . lines $ text

pairs :: [String] → [(String, [String])]
pairs = map pair where
  pair str =
    let lst = splitOn "\t" str
    in (head lst, tail lst)

process :: [String] → [String]
process xs = map stringify . customGroup . pairs $ xs where
  stringify (y, ys) = y ++ "\t" ++ (intercalate "\t" . sort $ ys)

customGroup :: (Eq a, Eq b, Ord a) ⇒ [(a, [b])] → [(a, [b])]
customGroup = map (\l → (fst . head $ l, nub (l >>= snd))) . groupBy ((==) `on` fst) . sortBy (comparing fst)
