module Maps where

doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (n:ns) = (2 * n) : doubleList ns

tripleList [] = []
tripleList (n:ns) = (3 * n) : tripleList ns

multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList _ [] = []
multiplyList m (n:ns) = (m * n) : multiplyList m ns

myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myNegate :: Num a => [a] -> [a]
myNegate = myMap (0-)

divisors :: Integer -> [Integer]
divisors p = [f | f <- [1..p], p `mod` f == 0]

divisorsList = myMap divisors

charToTuple :: Char -> (Int, Char)
charToTuple a = (1, a)

dropIfSame :: [(Int, Char)] -> [(Int, Char)]
dropIfSame [] = []
dropIfSame [a] = [a]
dropIfSame ((n1, c1):(n2,c2):ls)
  | c1 == c2 = dropIfSame ((n1 + n2, c2) : ls)
  | otherwise = (n1, c1) : dropIfSame ((n2,c2):ls)

encodeRLE :: String -> [(Int, Char)]
encodeRLE = dropIfSame . myMap charToTuple

tupleToString :: (Int, Char) -> String
tupleToString (n, c) = replicate n c

decodeRLE :: [(Int, Char)] -> String
decodeRLE = concat . myMap tupleToString
