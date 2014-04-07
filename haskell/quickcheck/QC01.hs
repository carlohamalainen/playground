module QC01 where

import Test.QuickCheck
import Data.List (intersperse)

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

split :: Char -> String -> [String]

-- First attempt, from tute page:
-- split _ [] = []

-- Fix:
split _ [] = [""]

split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs''= dropWhile (/=c) xs

join :: Char -> [String] -> String
join c = concat . intersperse [c]

prop_join_then_split :: Char -> String -> Bool
prop_join_then_split c xs = join c (split c xs) == xs

main :: IO ()
main = quickCheck prop_join_then_split


