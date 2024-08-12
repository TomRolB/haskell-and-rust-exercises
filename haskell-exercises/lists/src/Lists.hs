module Lists (member, union, intersection, difference,
              insert, insertionSort,
              binaryToDecimal, toDecimal, toDec, decimal, firsts,
              binaryAdd) where
  
import Data.Char(digitToInt)  

member:: Int -> [Int] -> Bool
member _ []      = False
member e (x:xs)  = e == x || member e xs


union:: [Int] -> [Int] -> [Int]
union [] ys     = ys
union (x:xs) ys 
  | member x ys = union xs ys
  | otherwise   = x : union xs ys

-- Remove Implementations, from, here on

intersection:: [Int] -> [Int] -> [Int]
intersection [] _ = []
intersection _ [] = []
intersection list1 list2 = [a | a <- list1, member a list2]
--intersection list1 list2 = fold member

difference:: [Int] -> [Int] -> [Int]
difference list1 list2 = [a | a <- list1, not (member a list2)]

insert:: Int -> [Int] -> [Int]
--insert n list
--  | n <= (head list) = n : list
--  | otherwise (head list) : (insert n (tail list))
insert n [] = n : []
insert n (x:xs)
  | n <= x = n : x : xs
  | otherwise = x : (insert n xs)

insertionSort :: [Int] -> [Int]
insertionSort list = foldl (flip insert) [] list

binaryToDecimal :: [Int] -> Int
--binaryToDecimal list = foldl (\acc (idx, x) -> acc + x * (2 ^ idx)) 0 (zip [0..] list)
--binaryToDecimal list = binaryToDecimalReversed $ reversed list
binaryToDecimal list = sum [x * (2 ^ idx) | (idx, x) <- zip [0..] (reverse list)]

toDecimal :: Int -> [Int] -> Int
toDecimal base list = sum [x * (base ^ idx) | (idx, x) <- zip [0..] (reverse list)]
    
toDec::Int -> String -> Int
toDec base s = toDecimal base (map digitToInt s)

-- Same as `toDec` But use a list comprehension

decimal::Int -> String -> Int
decimal base s = toDecimal base [digitToInt x | x <- s]

firsts::[a] -> [[a]]
firsts list = [take i list | (i, _) <- zip [1..] list]

-- Given two String that represents numbers in binary implement the 'binaryAdd' function
-- DO NOT USE a predefined '+' operation

binaryAdd::String -> String -> String
binaryAdd s1 s2 = ""