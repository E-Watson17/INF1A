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
split (a :#: b)   = split a ++ split b
split Sit         = []
split a           = [a]

-- 1b. join
join :: [Command] -> Command
join = foldr (:#:) Sit 

-- 1c. equivalent
-- equivalent 
equivalent a b = split a == split b

-- 1d. testing join and split
-- prop_split_join 
prop_split_join c = equivalent c ((join . split) c)

-- prop_split
prop_split c = and $ map (/= Sit) (split c)


-- 2a. copy
copy :: Int -> Command -> Command
copy 0 _ = Sit
copy num cmd = cmd :#: copy (num-1) cmd

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d num = copy num (Go d :#: Turn (360.0 / fromIntegral num))


-- 3. spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral _ 0 _ _                       = Sit
spiral side n step angle | side > 0  = Go side :#: Turn angle :#: spiral (side+step) (n-1) step angle
                         | otherwise = spiral (side+step) (n-1) step angle
-- 4. optimise
-- Remember that Go does not take negative arguments.

optimise :: Command -> Command
optimise = join' . optimise' . filter' . split where
    join' []   = Sit
    join' cmds = foldr1 (:#:) cmds
    filter' = filter (\a -> not $ elem a [Sit, Go 0, Turn 0])
    optimise' [] = []
    optimise' (Go a : Go b : cmds) = (optimise' . filter') (Go (a+b) : cmds)
    optimise' (Turn a : Turn b : cmds) = (optimise' . filter') (Turn (a+b) : cmds)
    optimise' (a : cmds) = a : (optimise' . filter') cmds


-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x where
    f 0 = Go 10
    f x = g(x-1) :#: p :#: f(x-1) :#: p :#: g(x-1)
    g 0 = Go 10
    g x = f(x-1) :#: n :#: g(x-1) :#: n :#: f(x-1)
    n   = Turn 60
    p   = Turn (-60)


-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n where
    f 0 = Go 10
    f x = f(x-1) :#: p :#: f(x-1) :#: n :#: n :#: f(x-1) :#: p :#: f(x-1)
    n   = Turn 60
    p   = Turn (-60)


-- 7. hilbert
hilbert :: Int -> Command
hilbert x = undefined

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