import Literal

data Clause a = Or[ Literal a ]
type Form a = [ Clause a ]

neg :: Literal a -> Literal a 
neg (P a) = N a
neg (N a) = P a

data Atom = A|B|C|D|W|X|Y|Z deriving Eq

eg = [ Or[N A, N C, P D], Or[P A, P C], Or[N D] ]

data Val a = And [ Literal a ] 
vals :: [a] -> [Val a] 
vals atoms = 
    let 
        vs [] = [[]] 
        vs (a:as) = [ P a: v | v <- vs as ] ++ [ N a: v | v <- vs as ] 
    in map And (vs atoms)

v :: Eq a => Literal a -> (Val a -> Bool) 
v lit (And gamma) = lit `elem` gamma
-- This is like our previous definition for |= but this speialised to the universal model 
-- gamma makes some delta true (entails) 

(|-) :: Eq a => Val a -> Clause a -> Bool 
And gamma |- Or delta = or [ d `elem` gamma | d <- delta ]

-- This is no the same as (|/-). It is (entails not), not (not entails) 
-- gamma makes every delta false 
(|-/) :: Eq a => Val a -> Clause a -> Bool 
And gamma |-/ Or delta = and [neg d `elem` gamma | d <- delta ]