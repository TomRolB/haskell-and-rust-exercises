module Frequencies  (Frequency, frequencyMap, frequencies, insert, insertionSort) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Tuple(swap)

type Frequency = (Int, Char)

frequencies::String -> [Frequency]
frequencies str = insertionSort $ map swap $ Map.toList $ frequencyMap str

frequencyMap::(Ord a) => [a] -> Map a Int
frequencyMap list = foldl countUp Map.empty list where
  countUp map char = Map.insertWith (+) char 1 map

insert::(Ord a) => a -> [a] -> [a]
insert e [] = [e]
insert e (x:xs)
  | e <= x = e : x : xs
  | otherwise = x : (insert e xs)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort list = foldl insert' [] list where
  insert' = flip insert