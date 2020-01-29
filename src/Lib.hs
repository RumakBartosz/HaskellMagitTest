module Lib
    ( toDigits, toDigitsRev, doubleEveryOther, splitInside, validate, sumDigits
    ) where

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse $ toDigits x

doubleEvery :: [Integer] -> [Integer]
doubleEvery [] = []
doubleEvery [x] = [x]
doubleEvery (x:y:xs) = x:(2*y): doubleEvery xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther x = reverse (doubleEvery (reverse x))

splitInside :: [Integer] -> [Integer]
splitInside [] = []
splitInside (x:xs) = if x >= 10
                     then x `div` 10 : x `mod` 10 : splitInside xs
                     else x : splitInside xs

sumDigits :: [Integer] -> Integer
sumDigits x = sum $ splitInside x

validate :: Integer -> Bool
validate x
  | x == 0 = False
  | otherwise = sumDigits (doubleEveryOther ( toDigits x)) `mod` 10 == 0

