-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 5
--
-- Week 5(14-18 Oct.)

module Tutorial5 where

import Data.Char
import Data.Ratio
import Test.QuickCheck
import Data.List

-- 1. Map

-- a.
doubles :: [Int] -> [Int]
doubles xs = map (*2) xs 

-- b.        
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map (\x -> fromIntegral x / 100) xs

-- c.
uppers :: String -> String
uppers xs = map toUpper xs

-- d.
uppersComp :: String -> String
uppersComp xs = [toUpper x | x <- xs]

prop_uppers :: String -> Bool
prop_uppers xs = uppers xs == uppersComp xs

-- 2. Filter
-- a.
alphas :: String -> String
alphas xs = filter isAlpha xs 

-- b.
above :: Int -> [Int] -> [Int]
above num xs = filter (>num) xs

-- c.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals xs = filter (\(a,b) -> a /= b) xs

-- d.
rmChar :: Char -> String -> String
rmChar ch str = filter (/=ch) str

-- e.
rmCharComp :: Char -> String -> String
rmCharComp ch str = [x | x <- str, ch /= x]

prop_rmChar :: Char -> String -> Bool
prop_rmChar ch str = rmChar ch str == rmCharComp ch str


-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map (*2) (filter (>3) xs)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = map reverse (filter (\x -> even (length x)) strs)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold xs = foldr (&&) True xs

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- b.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold xs = foldr (++) [] xs

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec [] str = str
rmCharsRec (x:xs) str = rmCharsRec xs (rmChar x str)

rmCharsFold :: String -> String -> String
rmCharsFold chs str = foldr rmChar str chs

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str


type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform xs = foldr (&&) True (map (== xs !! 0) xs)

-- b.
valid :: Matrix -> Bool
valid xs = uniform (map length xs) && length xs > 0 && length (xs !! 0) > 0


-- 6.
matrixWidth :: Matrix -> Int
matrixWidth m = length (m !! 0)

matrixHeight :: Matrix -> Int
matrixHeight m = length m

plusM :: Matrix -> Matrix -> Matrix
plusM a b
    | matrixWidth a /= matrixWidth b || matrixHeight a /= matrixHeight b || valid a == False || valid b == False = error("Invalid Matrix")
    | otherwise = zipWith (zipWith (+)) a b

-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM a b
    | matrixWidth a /= matrixHeight b || matrixHeight a /= matrixWidth b || valid a == False || valid b == False = error("Invalid Matrix")
    | otherwise = [[sum (zipWith (*) row col) | col <- transpose b] | row <- a]


timesM' :: Matrix -> Matrix -> Matrix 
timesM' a b = map (\m -> (map (\n -> sum (zipWith (*) m n)) (transpose b))) a


-- 8.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [uncurry f (x,y) | (x,y) <- zip xs ys]

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f) (zip xs ys)

-- -----------------------------------
-- -----------------------------------
-- -- Optional material
-- -----------------------------------
-- -----------------------------------
-- -- 9.

-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f m = map (map f) m

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
-- zipMatrix f m1 m2 = [map (uncurry f) (zip r1 r2) | (r1,r2) <- zip m1 m2]
zipMatrix f m1 m2 = zipWith (\r1 r2 -> map (uncurry f) (zip r1 r2)) m1 m2

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes m = [[ m !! j | j <- [0 .. (length m - 1)], i /= j]| i <- [0 .. (length m - 1)]]

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = map (\m' -> map transpose ((removes . transpose) m')) (removes m)

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = [[ (-1)^(i+j) | j <- [1 .. w]]| i <- [1 .. h]]
        
determinant :: Matrix -> Rational
determinant m
    | matrixWidth m == 2 && matrixHeight m == 2 = m!!0!!0 * m!!1!!1 - m!!0!!1 * m!!1!!0
    | matrixWidth m == 3 && matrixHeight m == 3 = sum (zipWith (\coeff minor -> coeff * (determinant minor)) (coeffs m) (minors m !! 0))
        where 
            coeffs :: Matrix -> [Rational]
            coeffs m = zipWith (*) (signMatrix (matrixWidth m) (matrixHeight m) !! 0) (m !! 0)

cofactors :: Matrix -> Matrix
cofactors m 
    | matrixWidth m == 2 && matrixHeight m == 2 = [[m!!1!!1,negate(m!!0!!1)],[negate(m!!1!!0),m!!0!!0]]
    | matrixWidth m == 3 && matrixHeight m == 3 = zipMatrix (*) (mapMatrix determinant ((transpose . minors) m)) (signMatrix (matrixWidth m) (matrixHeight m))
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k m = mapMatrix (*k) m

inverse :: Matrix -> Matrix
inverse m 
    | determinant m /= 0 = scaleMatrix (1/(determinant m)) (cofactors m)
    | otherwise = error ("No Inverse")

-- Tests
identity :: Int -> Matrix
identity n = [[ if w == h then 1 else 0 | w <- [1..n]] | h <- [1..n]]

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Bool
prop_inverse2 a b c d 
    | determinant m /= 0 = timesM m (inverse m) == identity 2
    | otherwise = True
        where
            m = [[a,b],[c,d]]

type Triple a = (a,a,a)
        
prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Bool
prop_inverse3 r1 r2 r3
    | determinant m /= 0 = timesM m (inverse m) == identity 3
    | otherwise = True
        where
            tupleToList (a,b,c) = [a,b,c]
            m = [tupleToList r1, tupleToList r2, tupleToList r3]

            