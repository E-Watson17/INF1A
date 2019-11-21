-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 9
--
-- Week 10(18-22 Nov.)
module Tutorial10 (Mat,from2DList,solveSudoku) where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Test.QuickCheck

type Row a     =  [a]
type Col a     =  [a]
type Matrix a  =  Col (Row a)
type Digit     =  Char
----------------------------- DATA ABSTRACTION ---------------------------------------------

data Mat = MkMat (Matrix Digit)
instance Show Mat where
    show (MkMat mat) = (foldr (++) "\n" . map ('\n':) . showGrid . map showRow) mat
    
from2DList :: [[Digit]] -> Mat
from2DList list | invariance list = MkMat list
                | otherwise       = error "Invalid 2D List"

invariance :: [[Digit]] -> Bool
invariance list = length list == 9 && all (\x->length x == 9) list && all (\x->elem x digits || blank x) (concat list)

solveSudoku :: Mat -> [Mat]
solveSudoku (MkMat mat) = map MkMat (search mat)

--------------------------------------------------------------------------------------------

digits :: [Digit]
digits =  ['1'..'9']

blank :: Digit -> Bool
blank d  =  d == ' '

-- 2.
group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy n xs
    | n >= length xs = [xs]
    | otherwise      = take n xs : groupBy n (drop n xs)

-- 3.
intersperse :: a -> [a] -> [a]
intersperse c []     = [c]
intersperse c (x:xs) = c : x : intersperse c xs

-- 4.
showRow :: String -> String
showRow = concat . intersperse "|" . group

-- 5.
showGrid :: Matrix Digit -> [String]
showGrid = concat . intersperse [replicate 13 '-'] . group

-- 6.
put :: Matrix Digit -> IO ()
put = foldr (>>) (return ()) . map putStrLn . showGrid . map showRow

-- 7.
choices :: Matrix Digit -> Matrix [Digit]
choices = map (map fillDigit) where
    fillDigit c | c == ' '  = digits
                | otherwise = [c]

-- 8.
cp :: [[a]] -> [[a]]
cp []        =  [[]]
cp (xs:xss)  =  [ x:ys | x <- xs, ys <- cp xss ]

prop_cp xss = length (cp xss) == product (map length xss)

expand :: Matrix [Digit] -> [Matrix Digit]
expand []     = [[]]
expand (r:rs) = [ xs : ys | xs <- cp r, ys <- expand rs ]

-- 9.
prop_expand rs = length (expand rs) == product (map length (concat rs))

-- 10.
answerLength :: Matrix Digit -> Integer
answerLength = product . map fromIntegral . map length . concat . choices

-- 11, 12, 13.
-- transpose :: [[a]] -> [[a]]
-- transpose [xs]      =  [[x] | x <- xs]
-- transpose (xs:xss)  =  zipWith (:) xs (transpose xss)

ungroup :: [[a]] -> [a]
ungroup =  undefined

rows, cols, boxs :: Matrix a -> Matrix a
rows m = m
cols m = transpose m
boxs m = map (concat . cols) (concat (map (group . cols) (group m)))

-- 14.
distinct :: Eq a => [a] -> Bool
distinct xs = xs == nub xs

-- 15.
valid :: Matrix Digit -> Bool
valid g  = and (map (\f -> and (map distinct (f g))) [rows, cols, boxs])

-- 16.
simple :: Matrix Digit -> [Matrix Digit]
simple =  filter valid . expand . choices

-- 17.
prop_id :: Bool
prop_id = and (map (\m -> (rows . rows) m == m && (cols . cols) m == m && (boxs . boxs) m == m) [easy,medium,hard,evil])

-- 18.
the :: [Digit] -> Digit
the [d]  =  d

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = pruneRow' 0 row where
    the [x] = x
    len = length row
    rowFilter :: Digit -> Row [Digit] -> Row [Digit]
    rowFilter c row = map (filter (/=c)) row
    pruneRow' :: Int -> Row [Digit] -> Row [Digit]
    pruneRow' i row | i < len && length (row!!i) == 1 = pruneRow' (i+1) (rowFilter (the(row!!i)) (take i row) ++ [row!!i] ++ rowFilter (the(row!!i)) (drop (i+1) row))
                    | i < len                         = pruneRow' (i+1) row
                    | otherwise = row

-- 19.
pruneBy :: (Matrix [Digit] -> Matrix [Digit])
             -> Matrix [Digit] -> Matrix [Digit]
pruneBy f  = f . map pruneRow . f

prune :: Matrix [Digit] -> Matrix [Digit]
prune m = foldr pruneBy m [rows, cols, boxs]

-- 20.
many :: Eq a => (a -> a) -> a -> a
many g x | x == g x  = x
         | otherwise = many g (g x)

-- 21.
extract :: Matrix [Digit] -> Matrix Digit
extract = map (map the) where
    the [x] = x
    the _   = undefined

-- 22.
solve :: Matrix Digit -> Matrix Digit
solve = extract . many prune . choices
-- only easy and medium can be solved 

-- 23.
failed :: Matrix [Digit] -> Bool
failed = or . map (=="") . concat 

-- 24.
solved :: Matrix [Digit] -> Bool
solved = and . map (\x -> length x == 1) . concat

-- 25.
shortest :: Matrix [Digit] -> Int
shortest = minimum . filter (>1) . map length . concat

-- 26.
-- break :: (a -> Bool) -> [a] -> ([a],[a])
-- break pred list = break' pred ([],list) where
--     break' :: (a -> Bool) -> ([a],[a]) -> ([a],[a])
--     break' pred (xs,[])                 = (xs,[])
--     break' pred (xs,y:ys) | pred y    = (xs,y:ys)
--                           | otherwise = break' pred (xs ++ [y],ys)

-- 27.
expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat = [preMat ++ [preRow ++ [[d]] ++ postRow] ++ postMat | d <- ds] where
    pred x = length x == shortest mat
    (preMat, row:postMat) = break (any pred) mat
    (preRow, ds:postRow) = break pred row


-- 28.
search :: Matrix Digit -> [Matrix Digit]
search = search' . many prune . choices where
    search' :: Matrix [Digit] -> [Matrix Digit]
    search' cs  | solved cs = [extract cs]
                | failed cs = []
                | otherwise = concat (map (search' . many prune) (expand1 cs))


-- Example from Bird

book    :: Matrix Digit
book    =  ["  4  57  ",
            "     94  ",
            "36      8",
            "72  6    ",
            "   4 2   ",
            "    8  93",
            "4      56",
            "  53     ",
            "  61  9  "]

-- Examples from websudoku.com

easy    :: Matrix Digit
easy    =  ["    345  ",
            "  89   3 ",
            "3    2789",
            "2 4  6815",
            "    4    ",
            "8765  4 2",
            "7523    6",
            " 1   79  ",
            "  942    "]

medium  :: Matrix Digit
medium  =  ["   4 6 9 ",
            "     3  5",
            "45     86",
            "6 2 74  1",
            "    9    ",
            "9  56 7 8",
            "71     64",
            "3  6     ",
            " 6 9 2   "]

hard    :: Matrix Digit
hard    =  ["9 3  42  ",
            "4 65     ",
            "  28     ",
            "     5  4",
            " 67 4 92 ",
            "1  9     ",
            "     87  ",
            "     94 3",
            "  83  6 1"]

evil    :: Matrix Digit
evil    =  ["  9      ",
            "384   5  ",
            "    4 3  ",
            "   1  27 ",
            "2  3 4  5",
            " 48  6   ",
            "  6 1    ",
            "  7   629",
            "     5   "]
br :: IO ()
br = putStrLn "***"

puts :: [Matrix Digit] -> IO ()
puts  =  sequence_ . map put

puzzle :: Matrix Digit -> IO ()
puzzle g  =  put g >>
             puts (search g) >>
             br
       
main =  puzzle easy >>
        puzzle medium >>
        puzzle hard >>
        puzzle evil

