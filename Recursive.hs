module Recursive where

myFactorial x
  | x < 0 = error "No negative number, please"
  | x == 0 = 1
  | x > 0 = x * myFactorial (x-1)

doubleFactorial x
  | x <= 1 = 1
  | otherwise = x * doubleFactorial (x - 2)

power _ 0 = 1
power 0 _ = 1
power n m = n * power n (m-1)

myReplicate :: Int -> a -> [a]
myReplicate n a
  | n <= 0 = []
  | otherwise = a : myReplicate (n - 1) a

myIndex _ [] = error "empty list"
myIndex n [x:xs]
