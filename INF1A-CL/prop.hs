import Data.List

data Literal a = P a | N a deriving (Eq, Show)
neg :: Literal a -> Literal a
neg (P a) = N a
neg (N a) = P a
data Clause  a = Or  [Literal a] deriving Show
data Form    a = And [Clause a] deriving Show

data Atom = A|B|C|D|E|F deriving (Eq, Show)

(<<) :: Eq a => [Clause a] -> Literal a -> [Clause a]
-- reduces the clauses with x set true
cs << x = [ Or (delete (neg x) c) | Or c <- cs, not (elem x c)]

models :: Eq a => [Clause a] -> [[Literal a]]
models clauses =
    case clauses of
        [] -> [[]] -- Empty Form
        Or [] : _ -> [] -- Empty Clause
        Or [x] : cs -> [x : m | m <- models (cs << x)] -- Unit Clause
        Or (x : xs) : cs -> [x : m | m <- models (cs << x)] ++ [neg x : m | m <- models (Or xs : (cs << neg x))] -- Other Clause
