module Huffman  (huffmanTrie, encode, decode, Trie(..), Bit(..)) where

import qualified Data.Map as M
import Frequencies (frequencies, insert)

data Bit = F | T deriving (Eq, Show)
type Bits = [Bit]  

data Trie a = Empty
            | Leaf a
            | Trie a :-: Trie a deriving (Eq, Show, Ord)

huffmanTrie::String -> Trie Char
huffmanTrie [] = Empty
huffmanTrie input = trie $ buildTrie charFrequencies where
  charFrequencies = map (\(k, char) -> (k, Leaf char) ) $ frequencies input
  buildTrie [] = error "No frequencies received"
  buildTrie [a] = a
  buildTrie (x:y:ls) = buildTrie $ insert (freq x + freq y, trie x :-: trie y) ls
  freq (a, _) = a
  trie (_, b) = b

encode :: String -> Trie Char -> Bits
encode str trie = concat $ map (encodeChar trie []) str where
  -- functors???
  encodeChar (Leaf a) path char = if a == char then Just path else Nothing
  encodeChar (l :-: r) path char = getJust encodeLeft encodeRight
  encodeLeft =  encodeChar l (F : path) char
  encodeRight = encodeChar r (T : path) char
  getJust Nothing Nothing = error "Invalid trie: no code found for character"
  getJust (Just _) (Just _) = error "Invalid trie: found more than one code for character"
  getJust (Just code) _ = code
  getJust _ (Just code) = code


decode::Bits -> Trie Char -> String
decode code trie = decode' code trie trie where
  decode' _ Empty _ = error "Found unmatched prefix"
  decode' [] (Leaf char) _ = [char]
  decode' [] _ _ = error "Last prefix is unmatched"
  decode' code' (Leaf char) root = char : decode' code' root root
  decode' (F:xs) (l :-: _) root = decode' xs l root
  decode' (T:xs) (_ :-: r) root = decode' xs r root

