module CollatzConjecture (collatz) where

--handleEven :: Integer -> Maybe Integer
--handleEven n
--  | nextStep == Nothing = Nothing
--  | otherwise 1 + nextStep
--    where nextStep = collatz (n `div` 2)

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | n == 1 = Just 0
  | isEven = (+1) <$> collatz (n `div` 2)
  | otherwise = (+1) <$> collatz (3 * n + 1)
    where isEven = n `mod` 2 == 0
