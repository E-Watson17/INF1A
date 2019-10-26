-- EXERCISE 1: BASIC TYPES IN HASKELL

-- True, False, 1, 2, 3.14, 'A', 'b', ' '
-- [True, True, False], [1,2,3], [1,2,3.14], ['A', 'b', ' '], "Ab "
-- (1,2), (1, 2, 3)
-- Just "Me", Nothing

-------------------------------------------------

-- EXERCISE 2: ROCK, PAPER, SCISSORS

data Move = Rock | Paper | Scissors deriving (Eq, Show)

-- Below is the definition of Ordering. 
-- Do NOT uncomment the line below!!!
-- data Ordering = LT | EQ | GT

order :: Move -> Move -> Ordering
order Rock Scissors = GT
order Scissors Paper = GT
order Paper Rock = GT
order Scissors Rock = LT
order Rock Paper = LT
order Paper Scissors = LT
order Rock Rock = EQ
order Scissors Scissors = EQ
order Paper Paper = EQ

-- Complete the rest of the function


-------------------------------------------------

-- EXERCISE 3: FRUITS AND THEIR COLOURS

-- 3.1 Uncomment the following 4 lines and complete the declarations
--
data Fruit = Strawberry | Plum | Pear | Mandarin | Grape | Apple | Banana deriving (Show)
data Colour = Red | Purple | Yellow | Green | Orange deriving (Show, Eq)
--
-- 3.2 Uncomment the following 10 lines and complete the function
--
colourOf :: Fruit -> Colour
colourOf Strawberry = Red 
colourOf Plum = Purple
colourOf Pear = Yellow
colourOf Mandarin = Orange
colourOf Grape = Green
colourOf Apple = Red
colourOf Banana = Yellow
--
-- 3.3 Uncomment all the following lines and complete the functions

isRedFruit :: Fruit -> Bool
isRedFruit fruit = colourOf fruit == Red

isPurpleFruit :: Fruit -> Bool
isPurpleFruit fruit = colourOf fruit == Purple

isYellowFruit :: Fruit -> Bool
isYellowFruit fruit = colourOf fruit == Yellow

isGreenFruit :: Fruit -> Bool
isGreenFruit fruit = colourOf fruit == Green

isOrangeFruit :: Fruit -> Bool
isOrangeFruit fruit = colourOf fruit == Orange

-------------------------------------------------

-- EXERCISE 4: An Universe of Discs and Triangles

data Thing = R | S | T | U | V | W | X | Y | Z deriving (Show)
                                                         
things :: [Thing]
things = [R, S, T, U, V, W, X, Y, Z]

isRed      :: Thing -> Bool
isBlue     :: Thing -> Bool
isGreen    :: Thing -> Bool
isDisc     :: Thing -> Bool
isTriangle :: Thing -> Bool
isSmall    :: Thing -> Bool
isBig      :: Thing -> Bool






 




-- Implementation Details
isRed U = True
isRed V = True
isRed _ = False

isBlue T = True
isBlue X = True
isBlue Z = True
isBlue _ = False

isGreen x = not (isRed x || isBlue x)

isDisc R = True
isDisc U = True
isDisc Y = True
isDisc Z = True
isDisc _ = False

isTriangle x = not (isDisc x)

isSmall R = True
isSmall S = True
isSmall V = True
isSmall X = True
isSmall _ = False

isBig t = not (isSmall t)


