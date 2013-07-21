{-# LANGUAGE UnicodeSyntax #-}
import Data.List
import Data.List.Split
import qualified Data.Map as M

data Edge a = Edge a a deriving Show

instance (Eq a) ⇒ Eq (Edge a) where
  Edge x1 y1 == Edge x2 y2 = x1 == x2 && y1 == y2

nlevel = 2

main :: IO ()
main = interact calculate

calculate :: String → String
calculate text = processGraph . convert . lines $ text

convert :: [String] → [Edge String]
convert xs = xs >>= convert' where
  convert' x =
    [Edge (head (res x)) (res x !! 1),
     Edge (head (res x)) (res x !! 1)]
  res        = splitOn "\t"

processGraph :: [Edge String] → String
processGraph edges = stringify $ M.map (verticesWithin nlevel) mapV where
                     vertices = nub $
                                edges >>= \x → case x of Edge a b → [a, b]
                     verticesWithin n v =
                       let adjacent = adjacencyMatrix M.! v
                           additional =
                             if n == 1 then []
                             else adjacent >>= (verticesWithin (n - 1))
                       in nub $ adjacent ++ additional
                     adjacencyMatrix = M.map findAdjacent mapV
                     findAdjacent v = edges >>= \x →
                       case x of Edge a b →
                                   if a == v then [b]
                                   else if b == v then [a]
                                        else []
                     mapV = M.fromList $ map (\x → (x, x)) vertices
                     stringify mp = unlines . M.elems $
                                    M.mapWithKey keyShow mp
                     keyShow key val = key ++ "\t" ++ intercalate "\t" val
