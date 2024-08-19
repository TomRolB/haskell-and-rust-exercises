module ListsPart2 (Bit(..), bitAt, charToBits, bits, queens, queenPossiblePositions) where

import Data.Char(ord)  
import Data.Bits(testBit)
  
data Bit = F | T  deriving (Eq, Show, Enum, Read)
type Bits = [Bit]

bitAt :: Int -> Char -> Bit
bitAt n c = if testBit (ord c) (7-n) then T else F 

charToBits :: Char -> Bits
charToBits c = [bitAt i c | i <- [0..7]]

bits::String -> Bits
--funca:
bits s = concat $ map charToBits s
--ahora con foldr:
--bits s = foldr (\acc char -> charToBits char)

type Solution = [Int]

queens::Int -> [Solution]
queens n = queensRecur n []

queenPossiblePositions::Int -> [Int] -> [Int]
queenPossiblePositions n [] = [1..n]
queenPossiblePositions n previousQueens = [col | col <- [1..n], not (col `elem` endangeredPositions)]
  where endangeredPositions = concat [[col - (len - row), col, col + (len - row)] | (row, col) <- zip [1..] previousQueens, let len = 1 + length previousQueens]


queensRecur::Int -> [Int] -> [Solution]
queensRecur n previousQueens
    | n == length previousQueens = [previousQueens]
    | length possiblePositions == 0 = []
    | otherwise = concat [queensRecur n queensStep | queensStep <- queensPossibleStep]
      where possiblePositions = queenPossiblePositions n previousQueens
            queensPossibleStep = [previousQueens ++ [pos] | pos <- possiblePositions]

-- queenPossiblePositions 4 ()