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


-- implement scanl and scanr with recursive and foldl/foldr
myScanlRecursive :: (a -> b -> a) -> a -> [b] -> [a]
myScanlRecursive f b [] = [b]
myScanlRecursive f b (x:xs) = b : myScanlRecursive f (f b x) xs

myScanlFoldl f b xs = foldl f' [b] xs
  where f' accs x = accs ++ (f (last accs) x) : []

myScanrRecursive :: (a -> b -> b) -> b -> [a] -> [b]
myScanrRecursive f b [] = [b]
myScanrRecursive f b (x:xs) = ( f x (head prev)) : prev
  where prev = myScanrRecursive f b xs

myScanrFoldr f b xs = foldr f' [b] xs
  where f' x accs = f x (head accs) : accs

-- implement factList with scanl1
factList :: Integer -> [Integer]
factList n = scanl1 (*) [1..n]


returnDivisible :: Int -> [Int] -> [Int]
returnDivisible n ns = [x | x <- ns, (mod x n) == 0]
