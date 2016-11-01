{- |
Module      :  TypesAndRecursion
Maintainer  :  alorozco.patriot53@gmail.com
Author      :  AlOrozco53
Stability   :  experimental
Portability :  portable
Version     :  0.2

Some examples for the 2017-1 Discrete Structures course.
Taught at Facultad de Ciencias, UNAM.
Topics mainly include natural numbers, lists, and geometry
recursive examples
-}

module TypesAndRecursion where


-- Natural numbers

data Nat = Zero | Succ Nat  deriving (Show, Eq)

-- Natural number addition
sumNat :: Nat -> Nat -> Nat
sumNat Zero m = m
sumNat (Succ n) m = sumNat n (Succ m) 
-- sumNat (Succ n) m = Succ $ sumNat n m

-- Decides if the first parameter n is STRICTLY greater than the
-- second parameter m; that is, n < m
greaterThan :: Nat -> Nat -> Bool
greaterThan Zero _ = False
greaterThan (Succ n) m = case m of
                           Zero -> True
                           Succ y -> greaterThan n y


-- A dummy natural number generator
numGenerator :: Int -> Nat
numGenerator n
  | n < 0 = error "cannot generate a non-negative integer out of a negative!!"
  | otherwise = head $ reverse $ take (n+1) (iterate Succ Zero)


-- Lists

-- Three implementation for the haskell "drop"
-- function. The first one uses Nat instead of Int.
myDrop :: Nat -> [a] -> [a]
myDrop Zero l = l
myDrop (Succ n) l = case l of
                      [] -> l
                      (_:xs) -> myDrop n xs

myDropLast1 :: Int -> [a] -> [a]
myDropLast1 n l = take ((length l) - n) l

myDropLast2 :: Nat -> [a] -> [a]
myDropLast2 n l = reverse p
  where p = myDrop n (reverse l)


-- Geometry

type Coord = (Double, Double)

type LineSegment = (Coord, Coord)

data Polygon = Triangle Coord Coord Coord
             | Rectangle Coord Coord Coord Coord
             | ConvexPolygon [LineSegment]
             deriving (Show, Eq)

-- Shows how to "parse" a ConvexPolygon to a Triangle
isRightTriangle :: Polygon -> Bool
isRightTriangle (Triangle _ _ _) = error "not my duty to implement this!"
isRightTriangle (Rectangle _ _ _ _) = error "not my duty to implement this!"
isRightTriangle (ConvexPolygon lineList) = if length lineList == 3
                                           then isRightTriangle $ Triangle v1 v2 v3
                                           else False
  where
    v1 = fst $ lineList !! 1
    v2 = fst $ lineList !! 2
    v3 = fst $ lineList !! 3
