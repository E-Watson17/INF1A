-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 2
--
-- Week 2(23-27 Sep.)

import Data.Char
import Data.List
import Test.QuickCheck
import Control.Monad (guard)


-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [div x 2 | x<-xs, even x]


-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)


-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensReference xs == halveEvens xs


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]


-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = length [x | x<-list, x>0]
-- 3(c) - list comprehension only returns a list of int, not int itself

-- 4. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = product (1 : [digitToInt x | x <- str, isDigit x])

countDigits :: String -> Int
countDigits str = length [x | x <- str, isDigit x]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs <= 9 ^ (countDigits xs)


-- 5. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = (toUpper x) : [toLower a | a<-xs]


-- 6. title

lowercase :: String -> String
lowercase xs = [toLower x | x<-xs]


-- List-comprehension version
title :: [String] -> [String]
title [] = []
title (x:xs) = (capitalise x) : [if (length a) >= 4 then (capitalise a) else (lowercase a) | a<-xs]

-- 7. signs

sign :: Int -> Char
sign i
    | i >= 1 && i <= 9 = '+'
    | i <= -1 && i >= -9 = '-'
    | i == 0 = '0'
    | otherwise = error "OVERFLOW"

signs :: [Int] -> String
signs xs = [sign x | x<-xs, x <= 9, x >= -9]


-- 8. score

score :: Char -> Int
score x
    | elem x "AEIOU" = 3
    | elem x "aeiou" = 2
    | elem x ['A' .. 'Z'] = 2
    | elem x ['a' .. 'z'] = 1
    | otherwise = 0

totalScore :: String -> Int
totalScore xs = product [score x | x <- xs, score x /= 0]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1

-- Tutorial Activity
-- 10. pennypincher

-- List-comprehension version.
pennypincher :: [Int] -> Int
pennypincher prices = round (0.9 * sum [fromIntegral price | price <- prices, 0.9 * fromIntegral price <= 19900.0])

-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs <= sum [x | x <- xs, x > 0]

-- Optional Material

-- 11. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [word | word <- words, length word == len, word !! pos == letter]


-- 12. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str goal = [pos | pos <- [0 .. (length str)-1], str !! pos == goal]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = length (search str goal) <= length str


-- 13. contains

contains :: String -> String -> Bool
contains str substr = or [isPrefixOf substr (drop x str) | x <- [0 .. length str]]

-- Depending on the property you want to test, you might want to change the type signature
prop_contains :: String -> String -> Bool
prop_contains str1 str2
    | contains str1 str2 == True = length str1 >= length str2
    | otherwise = True

