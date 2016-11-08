{- |
Module      :  Practica5Solution
Maintainer  :  alorozco.patriot53@gmail.com
Author      :  AlOrozco53
Stability   :  experimental
Portability :  portable
Version     :  0.2

Solution for the Práctica 5
2017-1 Discrete Structures
Facultad de Ciencias, UNAM
-}

module Practica5Solution where

import PicturesSVG

-- Ajedrez

row :: Int -> Picture -> Picture
row 0 _ = error "shit"
row 1 p = p
row n p = beside p (row (n-1) p)

column :: Int -> Picture -> Picture
column 0 _ = error "shit"
column 1 p = p
column n p = above p (column (n-1) p)

-- Constantes útiles

-- tablero con reinas y reyes
-- doubleDomino = above (beside comb1 comb2) (beside comb2 comb1)
-- vOffset = above comb1 comb2
-- hOffset = beside comb1 comb2
-- comb1 = over (invert queen) whiteSquare
-- comb2 = over king blackSquare

-- tablero normal
doubleDomino = above (beside whiteSquare blackSquare) (beside blackSquare whiteSquare)
vOffset = above whiteSquare blackSquare
hOffset = beside whiteSquare blackSquare

-- Tablero de ajedrez generalizado
grid :: Int -> Int -> Picture
grid 0 _ = error "shit"
grid _ 0 = error "shit"
grid 1 m = if odd m
           then above (column (div (m-1) 2) vOffset) whiteSquare
           else column (div m 2) vOffset
grid n 1 = if odd n
           then beside (row (div (n-1) 2) hOffset) whiteSquare
           else row (div n 2) hOffset
grid n m = if odd n
           then if odd m
                then addOffset (column (div (m-1) 2) (row (div (n-1) 2) doubleDomino)) m n
                else addOffset (column (div (m-1) 2) (row (div n 2) doubleDomino)) (-1) n
           else if odd m
                then addOffset (column (div m 2) (row (div (n-1) 2) doubleDomino)) m (-1)
                else column (div m 2) (row (div n 2) doubleDomino)
  where
    addOffset pic (-1) b = above pic (row (div b 2) hOffset)
    addOffset pic a (-1) = beside pic (column (div a 2) vOffset)
    addOffset pic a b = above (beside pic (column (div a 2) vOffset)) (row (div b 2) hOffset)

-- A basic picture mapping function, given another function and an image
picMapping :: (Picture -> Picture) -> Picture -> Picture
picMapping f p = case p of
                  Img image -> f $ Img image
                  Above p1 p2 -> Above (picMapping f p1) (picMapping f p2)
                  Beside p1 p2 -> Beside (picMapping f p1) (picMapping f p2)
                  Over p1 p2 -> Over (picMapping f p1) (picMapping f p2)
                  FlipH p -> FlipH $ picMapping f p
                  FlipV p -> FlipV $ picMapping f p
                  Invert p -> Invert $ picMapping f p

-- Geometría

type Coord = (Double, Double)

type LineSegment = (Coord, Coord)

data Polygon = Triangle Coord Coord Coord
             | Rectangle Coord Coord Coord Coord
             | ConvexPolygon [LineSegment]
             deriving (Show, Eq)

-- Decide si el polígono es triángulo
isTriangle :: Polygon -> Bool
isTriangle (Rectangle _ _ _ _) = False
isTriangle (Triangle a b c)
  | fst a == fst b && fst b == fst c = False
  | a == b = False
  | b == c = False
  | c == a = False
  | otherwise = not $ (m a b) == (m a c)
  where
    m s t = ((snd s) - (snd t)) / ((fst s) - (fst t))
isTriangle (ConvexPolygon lineList) = if length lineList == 3
                                           then isTriangle $ Triangle v1 v2 v3
                                           else False
  where
    v1 = fst $ lineList !! 1
    v2 = fst $ lineList !! 2
    v3 = fst $ lineList !! 3

-- Obtiene la distancia entre dos puntos en R^2
coordDistance :: Coord -> Coord -> Double
coordDistance (x,y) (w,z) = sqrt $ (abs $ w - x)^2 + (abs $ z - y)^2

-- Decide si el polígono es triángulo rectángulo
isRightTriangle :: Polygon -> Bool
isRightTriangle (Rectangle _ _ _ _) = False
isRightTriangle (Triangle a b c)
  | not $ isTriangle $ Triangle a b c = False
  | otherwise = or $ [x^2 + y^2 == (z^2 + 0.000000000000001),
                      y^2 + z^2 == (x^2 + 0.000000000000001),
                      x^2 + z^2 == (y^2 + 0.000000000000001),
                      x^2 + y^2 == z^2,
                      y^2 + z^2 == x^2,
                      x^2 + z^2 == y^2]
  where
    x = coordDistance a b
    y = coordDistance b c
    z = coordDistance c a
isRightTriangle (ConvexPolygon lineList) = if length lineList == 3
                                           then isRightTriangle $ Triangle v1 v2 v3
                                           else False
  where
    v1 = fst $ lineList !! 1
    v2 = fst $ lineList !! 2
    v3 = fst $ lineList !! 3

-- Realiza todos los triángulos posibles entre todos los vértices dados
makeTriangles :: [Coord] -> [Coord] -> [Coord] -> [Polygon]
makeTriangles xs ys zs = [Triangle x y z | x <- xs, y <- ys, z <- zs, isTriangle $ Triangle x y z]

-- Decide si el polígono dado es un cuadrado
isSquare :: Polygon -> Bool
isSquare (Triangle _ _ _) = False
isSquare (Rectangle a b c d)
  | fst a == fst b = (abs $ (snd a) - (snd b)) == (abs $ (fst a) - (fst d))
  | fst a == fst c = (abs $ (snd a) - (snd c)) == (abs $ (fst a) - (fst b))
  | fst a == fst d = (abs $ (snd a) - (snd d)) == (abs $ (fst a) - (fst c))
  | otherwise = False
isSquare (ConvexPolygon lineList) = if length lineList == 4
                                    then isSquare $ Rectangle v1 v2 v3 v4
                                    else False
  where
    v1 = fst $ lineList !! 1
    v2 = fst $ lineList !! 2
    v3 = fst $ lineList !! 3
    v4 = fst $ lineList !! 4

-- Decide a qué lado de la recta dada se encuentra el punto dado
pointSide :: LineSegment -> Coord -> Int
pointSide (a, b) (x, y) = if area < 0
                          then -1
                          else if area > 0
                               then 1
                               else 0
  where
    area = (ax * by) - (ay * bx)
    ax = (fst a) - x
    ay = (snd a) - y
    bx = (fst b) - x
    by = (snd b) - y

-- Decide si el punto dado está dentro del polígono CONVEXO dado
inConvexHull :: Polygon -> Coord -> Bool
inConvexHull (Rectangle _ _ _ _) _ = error "yet to implement!"
inConvexHull (Triangle a b c) p = inConvexHull (ConvexPolygon [(a, b), (b, c), (c, a)]) p
inConvexHull (ConvexPolygon lineList) p
  | null lineList = True
  | otherwise = aux sideList (head sideList)
  where
    sideList = [pointSide line p | line <- lineList]
    aux list firstElem = all (\x -> x == firstElem) list

-- Los k vecinos más cercanos
kNN :: Coord -> [(Coord, a)] -> Double -> [(Coord, a)]
kNN p list k = [neigh | neigh <- list, (coordDistance p (fst neigh)) <= k]

-- Listas

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake k list = case list of
                  [] -> []
                  (x:xs) -> [x] ++ (myTake (k-1) xs) 

myCount :: Eq a => a -> [a] -> Int
myCount _ [] = 0
myCount e (x:xs) = if x == e
                   then 1 + (myCount e xs)
                   else myCount e xs

myFreq :: Eq a => [a] -> [(a, Int)]
myFreq list = [(e, myCount e list) | e <- list]

dumbCompress :: String -> String
dumbCompress _ = error "missing implementation!"
