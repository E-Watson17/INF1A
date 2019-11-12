-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 9
--
-- Week 9(11-15 Nov.)
module Tutorial9 where

import Data.List
import Test.QuickCheck
import Data.Char


-- Type declarations

type FSM q = ([q], Alphabet, [q], [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      [0],
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      ['B'],
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [[0]],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> [q]
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]

states (k, _, _, _, _) = k
alph   (_, a, _, _, _) = a
start  (_, _, s, _, _) = s
final  (_, _, _, f, _) = f
trans  (_, _, _, _, t) = t


-- 2.
delta :: (Eq q) => FSM q -> [q] -> Char -> [q]
delta m s symbol = [ q' | (q, sym, q') <- trans m, x <- s, sym == symbol, x == q ]


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
-- accepts m str = or [ elem c (final m) | c <- result] where
--   result = foldl (delta m) (start m) str

accepts m xs = acceptsFrom m (start m) xs

acceptsFrom :: (Eq q) => FSM q -> [q] -> String -> Bool
acceptsFrom m q "" = or[ r `elem` final m | r <- q ]
acceptsFrom m q (x:xs) = acceptsFrom m (delta m q x) xs


-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical = sort . nub


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta m s symbol = canonical (delta m s symbol)

-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next m ss = canonical (ss ++ [ ddelta m s symbol | s <- ss, symbol <- alph m ])


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable m ss | ss == ss' = ss
               | otherwise = reachable m ss'
               where
                ss' = next m ss

-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal m ss = filter hasFinal ss where
  hasFinal s = or [ elem c (final m) | c <- s ]

-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans m ss = [ (s, symbol, ddelta m s symbol) | s <- ss, symbol <- alph m ]


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic m = (reachable m [start m], alph m, [start m], dfinal m (reachable m [start m]), dtrans m (reachable m [start m]))


-- Optional Material

-- QuickCheck

safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

--11.

charFSM :: Char -> FSM Bool
charFSM c = ([False, True], [c], [False], [True], [(False, c, True)])

emptyFSM :: FSM ()
emptyFSM = ([], [], [], [], [])

--12.

concatFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Either q q')
concatFSM a b = (ks, as, ss, fs, ts) where
  ks = map Left (states a) ++ map Right (states b)
  as = alph a ++ alph b
  ss = map Left (start a)
  fs = map Right (final b)
  ts = map (\(q,sym,q') -> (Left q,sym,Left q')) (trans a) ++ map (\(q,sym,q') -> (Right q,sym,Right q')) (trans b) ++ lk
  lk = [ (Left q, sym, Right s) | (q,sym,q') <- trans a, elem q' (final a), s <- start b ]

prop_concatFSM :: String -> String -> String -> Bool
prop_concatFSM m n o =
  accepts fsm (s ++ t)
  && (accepts fsm u == (s ++ t == u))
  where
  fsm = concatFSM a b
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o

--13.

intFSM :: Ord q => FSM q -> FSM Int
intFSM a = (ks, as, ss, fs, ts) where
  arr = zip (states a) [0..]
  ks  = map snd arr
  as  = alph a
  ss  = map (flip lookUp arr) (start a)
  fs  = map (flip lookUp arr) (final a)
  ts  = map (\(q,symbol,q') -> (lookUp q arr,symbol,lookUp q' arr)) (trans a)
  

lookUp :: Eq q =>  q -> [(q,Int)] ->  Int
lookUp q' qis =  the [ i | (q,i) <- qis, q == q' ]
  where
  the [q] = q

stringFSM :: String -> FSM Int
stringFSM str = intFSM ([0..length str],nub str,[0],[length str],[(q,c,q+1) | (q,c) <- zip [0..] str])

prop_stringFSM m n =
  accepts a s
  && accepts a t == (s == t)
  where
  a = stringFSM s
  s = safeString m
  t = safeString n

--14.

completeFSM :: (Ord q) => FSM q -> FSM (Maybe q)
completeFSM m = (ks, as, ss, fs, ts) where
  isMissing s sym = length [ q' | t@(q,symbol,q') <- trans m, symbol == sym, q == s ] == 0
  ks = Nothing : map Just (states m)
  as = alph m
  ss = map Just (start m)
  fs = map Just (final m)
  ts = [ (Just s,sym,Nothing) | s <- states m, sym <- alph m, isMissing s sym ] 
       ++ map (\(q,sym,q') -> (Just q,sym,Just q')) (trans m)
  
  

unionFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Maybe q, Maybe q')
unionFSM a b = (ks, as, ss, fs, ts) where
  a' = completeFSM a
  b' = completeFSM b
  ks = [(x,y) | x <- states a', y <- states b']
  as = nub (alph a' ++ alph b')
  ss = [(x,y) | x <- start a', y <- start b']
  fs = [(x,y) | x <- final a', y <- final b']
  ts = [((x1,y1),symx,(x2,y2))| (x1,symx,x2) <- trans a', (y1,symy,y2) <- trans b', symx == symy]
        
prop_unionFSM :: String -> String -> String -> Bool
prop_unionFSM m n o =
  accepts fsm u == (accepts a u || accepts b u)
  && accepts fsm s
  && accepts fsm t
  where
  fsm = unionFSM a b
  a = stringFSM s
  b = stringFSM t
  c = stringFSM u
  s = safeString m
  t = safeString n
  u = safeString o

--15.

star :: (Ord q) => FSM q -> FSM q
star = undefined

prop_star :: String -> Int -> Bool
prop_star m n =
  accepts fsm (concat (replicate i s))
  where
  fsm = star (stringFSM s)
  s = safeString m
  i = abs n

--16.

complementFSM :: (Ord q) => FSM q -> FSM (Maybe q)
complementFSM = undefined
           
prop_complement :: String -> String -> Bool
prop_complement m n =
  not (accepts fsm s)
  && accepts fsm t == not (s == t)
  where
  fsm = complementFSM (stringFSM s)
  s = safeString m
  t = safeString n

intersectFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (q,q')
intersectFSM = undefined
                
prop_intersectFSM1 m n =
  accepts fsm s
  && accepts fsm t == (s == t)
  where
  fsm = intersectFSM a a
  a = stringFSM s
  s = safeString m
  t = safeString n

prop_intersectFSM2 m n o =
  accepts fsm u == (accepts a u && accepts b u)
  where
  fsm = intersectFSM a b
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o

prop_intersectFSM3 m n o =
  accepts fsm s
  && accepts fsm u == accepts a u
  where
  fsm = intersectFSM a (unionFSM a b)
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o