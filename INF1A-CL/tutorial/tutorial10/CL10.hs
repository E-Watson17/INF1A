import CL7a
import CL9a 

count :: Wff Atom -> Int
count wff = length [ v | v <- envs [A,B,C,D,W,X,Y,Z] , undefined ]

eg4 = mkNFA qs as ts es ss fs where
  qs = [0..3]
  as = "12"
  ts = [ (0,'1',1), (0,'2',2), (1,'1',0), (1,'2',1)
       , (2,'2',0), (2,'1',3), (3,'1',2), (3,'2',3) ]
  es = [ (1,3), (3,1) ]
  ss = [0]
  fs = [0]

dfa4 = nfa2dfa eg4

-- regex for q4
w = S "1"     -- 1 wun
t = S "2"     -- 2 two
ts = Star t   -- 2*
r0 = Star (t:>:t) :>: Star(ts:>:w:>:ts:>:w:>:t)
r1 = Star (t:>:t) :>: Star(ts:>:w:>:ts:>:w:>:ts)
r2 = Star(ts:>:w:>:ts:>:w:>:ts) :>: Star (t:>:t) 

{-
NFA
  (fromList[fromList [0],fromList [0,2],fromList [1,3],fromList [2]])
  (fromList "12")
  [(fromList [0],'1',fromList [1,3]),  (fromList [0],'2',fromList [2])
  ,(fromList [0,2],'1',fromList [1,3]),(fromList [0,2],'2',fromList [0,2])
  ,(fromList [1,3],'1',fromList [0,2]),(fromList [1,3],'2',fromList [1,3])
  ,(fromList [2],'1',fromList [1,3]),  (fromList [2],'2',fromList [0])]
  [] (fromList [fromList [0]]) (fromList [fromList [0],fromList [0,2]])
-}
---- More Examples
m1 :: FSM Int
m1 = mkFSM qs as ts ss fs where
  qs = [0,1,2,3,4] 
  as = "ab"  
  ts = [ (0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2), (1,'b',4)
       , (2,'a',3), (2,'b',3), (3,'b',4), (4,'a',4), (4,'b',4) ]
  ss = [0]
  fs = [4]

m2 :: FSM Char
m2 = mkFSM qs as ts ss fs where
  qs = "ABCD"     
  as = "01"      
  ts = [ ('A', '0', 'D'), ('A', '1', 'B'), ('B', '0', 'A'), ('B', '1', 'C')
       , ('C', '0', 'B'), ('C', '1', 'D'), ('D', '0', 'D'), ('D', '1', 'D')]
  ss = "B"   
  fs = "ABC" 

dm1 :: FSM [Int] 
dm1 = mkFSM  qs as ts ss fs where
  qs = [[],[0],[1,2],[3],[3,4],[4]]
  as = "ab"                        
  ts = [ ([],   'a',[]),    ([],   'b',[])
       , ([0],  'a',[1,2]), ([0],  'b',[1,2])
       , ([1,2],'a',[3]),   ([1,2],'b',[3,4])
       , ([3],  'a',[]),    ([3],  'b',[4])
       , ([3,4],'a',[4]),   ([3,4],'b',[4])
       , ([4],  'a',[4]),   ([4],  'b',[4])]
  ss = [[0]]       
  fs =  [[3,4],[4]]

m3 :: NFA Int
m3 =  mkNFA qs as ts es ss fs where
  qs = [0..5]
  as = "ab"
  ts = [ (1,'a',2), (3,'b',4)]
  es = [ (0,1), (2,3), (0,5), (4,5), (4,1)]
  ss = [0]
  fs = [5]
