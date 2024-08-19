module Lists (member, union, intersection, difference,
              insert, insertionSort,
              binaryToDecimal, toDecimal, toDec, decimal, firsts,
              binaryAdd, merge, sort) where
  
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



xor:: String -> String -> String
xor x y
  | x /= y = "1"
  | otherwise = "0"

stringAnd:: String -> String -> String
stringAnd x y
  | x == "1" && y == "1" = "1"
  | otherwise = "0"

fullAdder::String -> String -> String -> (String, String)
fullAdder a b carryIn = (sum, carryOut)
  where
    sum = a `xor` b `xor` carryIn
    carryOut = (a `stringAnd` b) `xor` (carryIn `stringAnd` (a `xor` b))

binaryAddCarry::String -> String -> String -> String
binaryAddCarry [] [] "1" = "1"
binaryAddCarry [] [] "0" = ""
binaryAddCarry [] l2 carryIn = binaryAddCarry "0" l2 carryIn
binaryAddCarry l1 [] carryIn = binaryAddCarry l1 "0" carryIn
binaryAddCarry (x:xs) (y:ys) carryIn = sum : binaryAddCarry xs ys carryOut
  where (sum, carryOut) = fullAdder x y carryIn

binaryAdd::String -> String -> String
binaryAdd l1 l2 = reverse $ binaryAddCarry (reverse l1) (reverse l2) "0"
--binaryAdd s1 s2 = reverse $ binaryAddCarried (reverse s1) (reverse s2)
--binaryAddCarried::String -> String -> String -> String
--
--binaryAddCarried (x1:xs1) (x2:xs2) carry = ""

merge::(Ord a) => [a] -> [a] -> [a]
merge l1 [] = l1
merge [] l2 = l2
merge (x1:xs1) (x2:xs2)
  | x1 <= x2 = x1: (merge xs1 (x2:xs2))
  | otherwise = x2: (merge (x1:xs1) xs2)

sort::(Ord a) => [a] -> [a]
sort [] = []
sort [a] = [a]
sort list = merge (sort half1) (sort half2)
  where (half1, half2) = splitAt ((length list) `div` 2) list
