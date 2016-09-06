module Folds where

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc [] = acc
myFoldr f acc (x:xs) = f x $ myFoldr f acc xs

mySumr:: Num a => [a] -> a
mySumr = myFoldr (+) 0

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

echoesR, echoesL :: [Int] -> [Int]
echoesR = foldr (\ x xs -> (replicate x x) ++ xs) []
echoesL = foldl (\ xs x -> xs ++ (replicate x x)) []

myMapr f = foldr (\ x xs -> f x : xs) []
myMapl f = foldl (\ xs x -> xs ++ [f x]) []


myAnd :: [Bool] -> Bool
myAnd [] = error "empty list"
myAnd [a] = a
myAnd (x:xs) = x && (myAnd xs)

myAndFoldR :: [Bool] -> Bool
myAndFoldR = foldr (&&) True

myOr :: [Bool] -> Bool
myOr [] = error "empty list"
myOr [a] = a
myOr (x:xs) = x || (myOr xs)

myOrFoldR :: [Bool] -> Bool
myOrFoldR = foldr (||) False


myMaximumL, myMinimumL, myMaximumR, myMinimumR :: Ord a => [a] -> a
myMaximumL = foldl1 max
myMaximumR = foldr1 max
myMinimumL = foldl1 min
myMinimumR = foldr1 min

myReverseR, myReverseL :: [a] -> [a]
myReverseR = foldr (\ x xs -> xs ++ [x]) []
myReverseL = foldl (\ xs x -> x : xs) [] -- more efficient
