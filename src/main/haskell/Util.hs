{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Util where

import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Ord

class StringifyParse a where
  stringify :: a → String
  parse :: String → a

instance StringifyParse (String, [String]) where
  stringify = stringifyPair
  parse = parsePair

instance StringifyParse (String, [String], [String]) where
  stringify = stringifyTriple
  parse = parseTriple

parsePair :: String → (String, [String])
parsePair str =
    let lst = splitOn "\t" str
    in (head lst, tail lst)

parseTriple :: String → (String, [String], [String])
parseTriple str =
    let (h, adj:t) = parsePair str
        adjLst = splitOn "," . init . tail $ adj
    in (h, adjLst, t)

stringifyPair :: (String, [String]) → String
stringifyPair (y, ys) = y ++ "\t" ++ "["
                        ++ (intercalate "," . sort $ ys) ++ "]"

stringifyTriple :: (String, [String], [String]) → String
stringifyTriple (y, ys, [])  = stringifyPair (y, ys)
stringifyTriple (y, ys, yys) = stringifyPair (y, ys)
                               ++ "\t" ++ (intercalate "\t" yys)

finalStringify :: (String, [String], [String]) → String
finalStringify (y, ys, yys) =
  let lst = ys ++ yys
      sorted = sort . nub $ lst
  in y ++ "\t" ++ (intercalate "\t" sorted)

cycleThrough :: [a] → [[a]]
cycleThrough ys =
  let size = length ys
      lst  = cycle ys
  in map (\n → take size (drop n lst)) [0 .. size - 1]

customGroupPairs :: (Eq a, Eq b, Ord a) ⇒ [(a, [b])] → [(a, [b])]
customGroupPairs = map toTuple . customGroupTriples . map toTriple where
  toTriple (a, bs)    = (a, bs, [])
  toTuple (a, bs, []) = (a, bs)
  toTuple (a, bs, cs) = (a, bs ++ cs) -- should not get here, still let's be safe

customGroupTriples :: (Eq a, Eq b, Eq c, Ord a) ⇒
                     [(a, [b], [c])] → [(a, [b], [c])]
customGroupTriples = map transform . groupBy ((==) `on` first)
                     . sortBy (comparing first) where
  transform xs = (first . head $ xs,
                  nub (xs >>= second),
                  nub (xs >>= third))
  first  (a, _, _) = a
  second (_, b, _) = b
  third  (_, _, c) = c
