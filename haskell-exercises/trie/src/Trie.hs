module Trie  (Trie(..), left, right, find, decode, toList) where

import Bit
  
data Trie a = Leaf a
            | Trie a :-: Trie a deriving (Eq, Show)
            
left::Trie a -> Trie a
left (leftTrie :-: _) = leftTrie

right::Trie a -> Trie a
right ( _ :-: rightTrie) = rightTrie
  
find::Bits -> Trie a -> a
find bits trie
  | length bitsLeft == 0 = value
  | otherwise = error "There's nothing for this key"
  where (value, bitsLeft) = findSafe bits trie
--find [] (Leaf a) = a
--find [] (_ :-: _) = error "There's nothing for this key"
--find _ (Leaf a) = error "There's nothing for this key"
--find (F:xs) (l :-: _) = find xs l
--find (T:xs) (_ :-: r) = find xs r

-- Same than find, but returning the leaf's value even
-- if the list has not been depleted
findSafe::Bits -> Trie a -> (a, Bits)
findSafe list (Leaf a) = (a, list)
findSafe [] (_ :-: _) = error "There's nothing for this key"
findSafe (F:xs) (l :-: _) = findSafe xs l
findSafe (T:xs) (_ :-: r) = findSafe xs r

--findSafeRecur::Bits -> Trie a -> (a, Bits)
--findSafeRecur list (Leaf a)
--  |
--  |
--  where

decode::Bits -> Trie Char -> String
--decode bits trie = foldl
decode [] trie = ""
decode bits trie = char : decode bitsLeft trie
  where (char, bitsLeft) = findSafe bits trie

toList::Trie a -> [(a, Bits)]
toList trie = error "Implement it"

toListRecur
