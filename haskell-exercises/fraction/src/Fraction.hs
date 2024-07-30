module Fraction (Fraction, add, sub, mul, divide, hcf) where

type Fraction = (Int, Int)

numerator :: Fraction -> Int
numerator (num, _) = num

denominator :: Fraction -> Int
denominator (_, denom) = denom

-- Implement the `add` Function

add :: Fraction -> Fraction -> Fraction
add n d = ((cross_multiplied_n + cross_multiplied_d), denominator n * denominator d)
    where cross_multiplied_n = numerator n * denominator d
          cross_multiplied_d = numerator d * denominator n

-- Implement the `sub` Function

sub :: Fraction -> Fraction -> Fraction
sub n d = ((cross_multiplied_n - cross_multiplied_d), denominator n * denominator d)
    where cross_multiplied_n = numerator n * denominator d
          cross_multiplied_d = numerator d * denominator n

-- Implement the `mul` Function

mul :: Fraction -> Fraction -> Fraction
mul n d = ((numerator n * numerator d), (denominator n * denominator d))

-- Implement the `divide` Function

divide :: Fraction -> Fraction -> Fraction
divide n d = ((numerator n * denominator d), (denominator n * numerator d))

-- Implement the `hcf` Function

--min :: Int -> Int -> Int
--min n d
--  | n < d = n
--  | otherwise = d

--is_hcf :: Int -> Int -> Int -> Int
--is_hcf n d h
--  | mod n h == 0 && mod d h == 0 = h
--  | otherwise = is_hcf n d (h-1)

hcf :: Int -> Int -> Int
hcf n d
  | d == 0 = n
  | otherwise = hcf d (n `mod` d)
