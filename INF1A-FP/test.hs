import Test.QuickCheck
import Data.List

-- Proposition for CL


type Name = String
data Prop =
    Var Name
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    deriving (Eq, Ord)

type Names = [Name]
type Env = [(Name,Bool)]

par :: String -> String
par s = "(" ++ s ++ ")"

showProp :: Prop -> String
showProp (Var x) = x
showProp F = "F"
showProp T = "T"
showProp (Not p) = par ("~" ++ showProp p)
showProp (p :|: q) = par (showProp p ++ "|" ++ showProp q)
showProp (p :&: q) = par (showProp p ++ "&" ++ showProp q)

names :: Prop -> Names
names (Var x) = [x]
names F = []
names T = []
names (Not p) = names p
names (p :|: q) = nub (names p ++ names q)
names (p :&: q) = nub (names p ++ names q)

eval :: Env -> Prop -> Bool
eval e (Var x) = lookUp e x
eval e F = False
eval e T = True
eval e (Not p) = not (eval e p)
eval e (p :|: q) = eval e p || eval e q
eval e (p :&: q) = eval e p && eval e q

lookUp :: Eq a => [(a,b)] -> a -> b
lookUp xys x = head [y | (x',y) <- xys, x == x']

envs :: Names -> [Env]
envs [] = [[]]
envs (x:xs) = map ((x,False):) (envs xs) ++ map ((x,True):) (envs xs)

satisfiable :: Prop -> Bool
satisfiable p = or [eval e p | e <- envs (names p)]

-- Propositions and Environments
p0 :: Prop 
p0 = (Var "a" :&: Not (Var "a"))
e0 :: Env 
e0 = [("a",True)]
p1 :: Prop 
p1 = (Var "a" :&: Var "b") :|: (Not (Var "a") :&: Not (Var "b"))
e1 :: Env 
e1 = [("a",False), ("b",False)]



-- Binary Tree
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
