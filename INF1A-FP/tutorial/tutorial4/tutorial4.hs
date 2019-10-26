-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 4
--
-- Week 4(07-11 Oct.)

module Tutoria4 where

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:irene.vp@ed.ac.uk\">Irene Vlassi</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br><b>TA:</b> "
            , "mailto:irene.vp@ed.ac.uk\">Irene Vlassi</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Philip Wadler","wadler@inf.ed.ac.uk")
               , ("Irene Vlassi","irene.vp@ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString str1 str2 = (map toLower str1) == (map toLower str2)


-- 2.
prefix :: String -> String -> Bool
prefix substr str = sameString substr (take (length substr) str)

prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n =  prefix substr (map toLower str) &&
                         prefix substr (map toUpper str)
                           where
                             substr  =  take n str
-- This function is not sufficient enough because I can make n larger than (length str)

prop_prefix_neg :: String -> Int -> Bool
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
                          where substr = take n str
        
        
-- 3.
contains :: String -> String -> Bool
contains str substr = or [prefix substr (drop x str) | x <- [0 .. length str]]

prop_contains :: String -> Int -> Int -> Bool
prop_contains str a b = contains (map toLower str) substr && contains (map toUpper str) substr
                            where
                              substr = (take a (drop b str))


-- 4.
takeUntil :: String -> String -> String
takeUntil target str = helperTakeUntil str target 0
                            where
                              helperTakeUntil :: String -> String -> Int -> String 
                              helperTakeUntil s t i
                                | length s < i = []
                                | contains (take i s) t = (take (i - length t) s)
                                | otherwise = helperTakeUntil s t (i + 1)

dropUntil :: String -> String -> String
dropUntil target str = drop (length (takeUntil target str) + length target) str


-- 5.
split :: String -> String -> [String]
split s str
  | contains str s = (takeUntil s str) : split s (dropUntil s str)
  | otherwise = str : []

reconstruct :: String -> [String] -> String
reconstruct s (x:xs)
  | xs == [] = x
  | otherwise = x ++ s ++ reconstruct s xs

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML web = tail (split "<a href=\"" web)

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails links = [link | link <- links, prefix "mailto:" link]


-- 8.
link2pair :: Link -> (Name, Email)
link2pair link = (dropUntil "\">" (takeUntil "</a>" link) , dropUntil "mailto:" (takeUntil "\">" link))


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML web = nub [link2pair link | link <- takeEmails(linksFromHTML web)]

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail name book = [(a,b) | (a,b) <- book, contains a name]


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML web name = findEmail name (emailsFromHTML web)


-- Optional Material

-- 13.
hasInitials :: String -> Name -> Bool
hasInitials str name
  | length str /= length (split " " name) = False
  | otherwise = and [toLower a == toLower (head b) | (a,b) <- zip str (split " " name)]

-- 14.
emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name, Email)]
emailsByMatchFromHTML f web = [(x,y) | (x,y) <- emailsFromHTML web, f x]

emailsByInitialsFromHTML :: String -> HTML -> [(Name, Email)]
emailsByInitialsFromHTML str web = emailsByMatchFromHTML (hasInitials str) web

-- 15.

-- If your criteria use parameters (like hasInitials), change the type signature.
myCriteria :: Name -> Bool
myCriteria name = hasInitials "PW" name

emailsByMyCriteriaFromHTML :: HTML -> [(Name, Email)]
emailsByMyCriteriaFromHTML web = emailsByMatchFromHTML myCriteria web

-- 16.
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ dropUntil " " name ++ ", " ++ takeUntil " " name ++ "\t\t" ++ email | (name,email) <- addr ]
