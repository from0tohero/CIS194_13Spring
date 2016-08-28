{-# OPTIONS_GHC -Wall #-}

-- Converts an integer to a list of digits, e.g 12345 -> [1,2,3,4,5]
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = let toDigits_ n_ acc
                      | n_ `div` 10 == 0 = n_ : acc
                      | otherwise = toDigits_ (n_ `div` 10) ((n_ `mod` 10) : acc)
                in toDigits_ n []

-- Converts an integer to a list of digits in reverse order, 
-- e.g 12345 -> [5,4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Doubles every second number from right, e.g. [8,7,6,5] -> [16,7,12,5]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = let
  -- Doubles every second number from left and reverse it, 
  doubleEveryOther_ :: [Integer] -> [Integer] -> [Integer]
  doubleEveryOther_ acc (h1:(h2:t)) = doubleEveryOther_ ([2*h2,h1] ++ acc) t
  doubleEveryOther_ acc [x] = x : acc
  doubleEveryOther_ acc [] = acc
  in doubleEveryOther_ [] (reverse list)

-- Calculates the sum of all digits.
sumDigits :: [Integer] -> Integer
sumDigits list = let 
  sumDigits_ :: Integer -> [Integer] -> Integer
  sumDigits_ acc [] = acc
  sumDigits_ acc (h:t) = sumDigits_ (acc + flatten 0 h) t where
    flatten acc_ x
      | x `div` 10 == 0 = x + acc_
      | otherwise = flatten (acc_ + x `mod` 10) (x `div` 10)
  in sumDigits_ 0 list

validate :: Integer -> Bool
validate number
  | mySum `mod` 10 == 0 = True
  | otherwise = False
  where mySum = sumDigits (doubleEveryOther (toDigits number))

-- Ex 5
type Peg = String
type Move = (Peg, Peg)
-- number of discs, origin, destination, middle
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 ori dest _ = [(ori, dest)]
hanoi n ori dest mid = hanoi (n-1) ori mid dest ++ [(ori, dest)] ++ hanoi (n-1) mid dest ori

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
--hanoi4 2 ori dest mid1  _= [(ori, mid1), (ori, dest)]
hanoi4 1 ori dest _ _ = [(ori, dest)]
hanoi4 2 ori dest mid1 _ = [(ori, mid1), (ori, dest), (mid1, dest)]
hanoi4 n ori dest mid1 mid2 = hanoi4 (n-2) ori mid1 mid2 dest ++ hanoi4 2 ori dest mid2 mid1 ++ hanoi4 (n-2) mid1 dest ori mid1
