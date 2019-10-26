-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 3
--
-- Week 3(30-04 Oct.)
module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck

import Data.Function
import Data.Maybe


-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
      | even x = (div x 2) : halveEvensRec xs
      | otherwise = halveEvensRec xs

-- halveEvens :: [Int] -> [Int]
-- halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

halveEvens :: [Int] -> [Int]
halveEvens xs = [div x 2 | x<-xs, even x]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensRec xs == halveEvens xs


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec lo hi (x:xs)
      | x >= lo && x <= hi = x : inRangeRec lo hi xs
      | otherwise = inRangeRec lo hi xs

-- inRange :: Int -> Int -> [Int] -> [Int]
-- inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRangeRec lo hi xs == inRange lo hi xs


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs)
      | x > 0 = 1 + countPositivesRec xs
      | otherwise = countPositivesRec xs

-- countPositives :: [Int] -> Int
-- countPositives list = length [x | x <- list, x > 0]

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives l = countPositivesRec l == countPositives l


-- 4.

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs)
      | isDigit x = (digitToInt x) * (multDigitsRec xs)
      | otherwise = multDigitsRec xs

-- multDigits :: String -> Int
-- multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

multDigits :: String -> Int
multDigits str = product (1 : [digitToInt x | x <- str, isDigit x])

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigitsRec xs == multDigits xs


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================


-- 5.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch xs
      | (length [b | (a,b) <- xs, ch == a]) == 0 = ch
      | otherwise = head [b | (a,b) <- xs, ch == a]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch [] = ch
lookUpRec ch ((a,b):xs)
      | a == ch = b
      | otherwise = lookUpRec ch xs


prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = lookUpRec c k == lookUp c k


-- 6.

encipher :: Int -> Char -> Char
encipher k ch = lookUp ch (makeKey k)


-- 7.

normalize :: String -> String
normalize str = [toUpper x | x <- str, isAlpha x || isDigit x] 


encipherStr :: Int -> String -> String
encipherStr k str = [(encipher k ch) | ch <- (normalize str)]


-- 8.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey key = [(b,a) | (a,b) <- key]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((a,b):xs) = (b,a) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey key = reverseKeyRec key == reverseKey key


-- 9.

decipher :: Int -> Char -> Char
decipher k ch = lookUp ch (reverseKey (makeKey k))

decipherStr :: Int -> String -> String
decipherStr k str = [(decipher k ch) | ch <- str, elem ch (['A'..'Z'] ++ "1234567890 ")]

-- Optional Material
-- =================


-- 10.

contains :: String -> String -> Bool
contains str substr = or [isPrefixOf substr (drop x str) | x <- [0 .. length str]]


-- 11.

candidates :: String -> [(Int, String)]
candidates str = [(key, (decipherStr key str)) | key <- [0 .. 25], contains (decipherStr key str) "THE" || contains (decipherStr key str) "AND"]


-- 12.

splitEachFive :: String -> [String]
splitEachFive str
      | length str >= 5 = (take 5 str) : (splitEachFive (drop 5 str))
      | length str > 0 = splitEachFive (str ++ "X")
      | otherwise = []

prop_transpose :: String -> Bool
prop_transpose str = transpose (transpose (splitEachFive str)) == splitEachFive str


-- 13.
encrypt :: Int -> String -> String
encrypt k str = concat (transpose (splitEachFive (encipherStr k str)))


-- 14.
splitIntoFive :: String -> [String]
splitIntoFive str = [[str !! a | a <- [(b * (div (length str) 5)) .. ((b + 1) * (div (length str) 5) - 1)]] | b <- [0 .. 4]]

decrypt :: Int -> String -> String
decrypt k str = decipherStr k (concat (transpose (splitIntoFive str)))