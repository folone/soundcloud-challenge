{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}
import           Data.List
import           Data.List.Split
import qualified Data.Map        as M

data Edge a = Edge a a deriving Show

class Stringify a where
  stringify :: a → String

instance (Eq a) ⇒ Eq (Edge a) where
  Edge x1 y1 == Edge x2 y2 = x1 == x2 && y1 == y2

instance Stringify String where
  stringify = id

instance Stringify ((String, [String])) where
  stringify (key, val) = stringify key ++ "\t" ++ intercalate "\t" (map stringify val)

nlevel = 2

main :: IO ()
main = interact calculate

calculate :: String → String
calculate text = unlines . processGraph . convert . lines $ text

convert :: [String] → [Edge String]
convert xs = xs >>= convert' where
  convert' x =
    [Edge (head (res x)) (res x !! 1),
     Edge (head (res x)) (res x !! 1)]
  res        = splitOn "\t"

processGraph :: [Edge String] → [String]
processGraph edges = M.elems . M.mapWithKey (curry stringify) $
                     M.map (verticesWithin nlevel) mapV where
                     verticesWithin n v =
                       let findAdjacent v = edges >>= \x →
                             case x of Edge a b | a == v    → [b]
                                                | b == v    → [a]
                                                | otherwise → []
                           adjacent = adjacencyMatrix M.! v
                           adjacencyMatrix = M.map findAdjacent mapV
                           additional =
                             if n == 1 then []
                             else adjacent >>= verticesWithin (n - 1)
                       in nub $ adjacent ++ additional
                     mapV =
                       let vertices = nub $
                                      edges >>= \x → case x of Edge a b → [a, b]
                       in M.fromList $ map (\x → (x, x)) vertices
