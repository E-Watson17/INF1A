-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 8
--
-- Week 8(04-08 Nov.)

module Tutorial8 where

import System.Random


-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen xs = maximum [length b | (a,(b,i)) <- xs] 

formatLine :: Int -> (Barcode, Item) -> String
formatLine num (barcode, (product, unit)) = barcode ++ "..." ++ (fixed num product) ++ "..." ++ unit where
  fixed :: Int -> String -> String
  fixed num str | length str < num   = fixed num (str ++ ".") 
                | otherwise          = str

showCatalogue :: Catalogue -> String
showCatalogue cs = concat [formatLine 100 c ++ "\n" | c <- toList cs]
     
-- Exercise 2
-- a) The return type is Maybe Item. Return values are all Just Item and Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Nothing) = []
maybeToList (Just x)  = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just (head xs)

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [x | Just x <- xs]

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems codes cat = catMaybes [get code cat | code <- codes]


-- Exercise 4
 
-- For Exercises 6-10 check KeymapTree.hs 

-- Exercise 12

-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
