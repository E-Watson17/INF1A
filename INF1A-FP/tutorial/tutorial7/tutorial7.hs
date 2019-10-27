-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 7
--
-- Week 7(29 Oct.- 01 Nov.)

-- module Main where

import LSystem
import Test.QuickCheck

pathExample = (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#:  Go 30)

-- 1a. split
split :: Command -> [Command]
split = undefined

-- 1b. join
join :: [Command] -> Command
join = undefined

-- 1c. equivalent
-- equivalent 
equivalent = undefined

-- 1d. testing join and split
-- prop_split_join 
prop_split_join = undefined

-- prop_split
prop_split = undefined


-- 2a. copy
copy :: Int -> Command -> Command
copy = undefined

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon = undefined

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon = undefined


-- 3. spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral = undefined


-- 4. optimise
-- Remember that Go does not take negative arguments.

optimise :: Command -> Command
optimise = undefined

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead = undefined


-- 6. snowflake
snowflake :: Int -> Command
snowflake = undefined


-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined

--------------------------------------------------
--------------------------------------------------
---------------- Optional Material ---------------
--------------------------------------------------
--------------------------------------------------

-- Bonus L-Systems

peanoGosper = undefined


cross = undefined


branch = undefined

thirtytwo = undefined

main :: IO ()
main = display pathExample