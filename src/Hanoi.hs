module Hanoi
  ( hanoi, Peg, hanoi'
  ) where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1 = [(a,b)]
  | otherwise = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' n a b c d
  | n == 1 = [(a,b)]
  | otherwise = hanoi' (n-2) a c d b ++ [(a,c),(a,b),(d,b)] ++ hanoi' (n-2) c b a d
