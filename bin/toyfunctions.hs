{- |
Module      :  ToyFunctions
Maintainer  :  alorozco.patriot53@gmail.com
Author      :  AlOrozco53
Stability   :  experimental
Portability :  portable
Version     :  0.0

Some examples of toy functions discussed
for the 2017-1 Discrete Structures course
Facultad de Ciencias, UNAM
--}

module ToyFunctions where

import PicturesSVG

-- Takes a list of strings and builds up a list
-- with 2-tuples, where the first component is the last
-- character of each string in the given list and the
-- second one is an integer i that tells that
-- the first component comes from the ith element in
-- the given list
magic :: [String] -> [(String, Int)]
magic phrase = zip suffixes [1..length $ phrase]
  where
    suffixes = [take 1 (reverse x) | x <- phrase]

-- Takes a list of strings, and an integer k.
-- Builds up a list with 2-tuples, where the first component
-- is the suffix of each string in l with length (at most) k.
-- The second one is an integer i that tells that
-- the first component comes from the ith element in
-- the given list.
magicK :: [String] -> Int -> [(String, Int)]
magicK phrase k = zip suffixes [1..length $ phrase]
  where
    suffixes = [reverse $ take k (reverse x) | x <- phrase]

-- Same functionality as the 'words'
-- function, though only with one char given as
-- delimeter.
-- I.e. split string char == words string
split :: String -> Char -> [String]
split s c = splitAux s c []
  where
    splitAux [] _ carry = [carry]
    splitAux (x:xs) c carry = if x == c
                              then [carry] ++ (splitAux xs c [])
                              else splitAux xs c (carry ++ [x])

-- Gets the head of a list
myHead :: [a] -> a
myHead [] = error "list is empty!"
myHead (x:xs) = x

-- Gets the tail of a list
myTail :: [a] -> [a]
myTail [] = error "list is empty!"
myTail (x:xs) = xs

-- Appends two lists (concatenation)
myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

-- Miran Lipovaca's version of the list length function
-- As found in his "Learn you a Haskell for a Great Good!"
length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

-- Given a well written (title) string,
-- builds the list with its initials
-- (upper-cased letters).
initials :: String -> String
initials title = [x | x <- title, elem x ['A'..'Z']]

-- Pictures

-- A basic picture filter given a function and an image
picFilter :: (Picture -> Picture) -> Picture -> Picture
picFilter f p = case p of
                  Img image -> f $ Img image
                  Above p1 p2 -> Above (picFilter f p1) (picFilter f p2)
                  Beside p1 p2 -> Beside (picFilter f p1) (picFilter f p2)
                  Over p1 p2 -> Over (picFilter f p1) (picFilter f p2)
                  FlipH p -> FlipH $ picFilter f p
                  FlipV p -> FlipV $ picFilter f p
                  Invert p -> Invert $ picFilter f p
