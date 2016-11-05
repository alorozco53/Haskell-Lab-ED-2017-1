{- |
Module      :  BTInstGeom
Maintainer  :  alorozco.patriot53@gmail.com
Author      :  AlOrozco53
Stability   :  experimental
Portability :  portable
Version     :  1.1

Some examples for the 2017-1 Discrete Structures course.
Taught at Facultad de Ciencias, UNAM.
Topics mainly include binary trees, instance declarations,
and geometry
-}

module BTInstGeom where

-- My List implementation

data MyList a = Nil | Cons a (MyList a)

-- allow MyList to be printed
instance Show a => Show (MyList a) where
  show = showList2

-- a "pretty printing" show function
showList1 :: Show a => MyList a -> String
showList1 Nil = "[]"
showList1 (Cons x xs) = show x ++ " :(" ++ showList1 xs ++ ")"

-- a more "pretty printing" show function
showList2 :: Show a => MyList a -> String
showList2 Nil = "[]"
showList2 (Cons x xs) = "[" ++ show x ++ showAux xs ++ "]"
  where
    showAux Nil = ""
    showAux (Cons y ys) = "," ++ show y ++ showAux ys


-- Binary trees

-- Binary Tree type declaration
data BinTree a = BTNil
               | BTBranch a (BinTree a) (BinTree a)
               deriving Eq

-- make a binary tree instance of the Show class
instance Show a => Show (BinTree a) where
  show btree = showAux btree ""
    where
      showAux BTNil acc = "Nil"
      showAux (BTBranch r lchild rchild) acc = let newacc = acc ++ "\t"
                                               in "Branch " ++ show r ++
                                                  "\n" ++ newacc ++ showAux rchild newacc ++
                                                  "\n" ++ newacc ++ showAux lchild newacc

-- Gives the height of a binary tree
heightBT :: BinTree a -> Int
heightBT BTNil = 0
heightBT (BTBranch x lchild rchild) = 1 + max (heightBT lchild) (heightBT rchild)

-- Insert an element in a "binary search tree" fashion
insertBST :: (Ord a) =>  a -> BinTree a -> BinTree a
insertBST x BTNil = BTBranch x BTNil BTNil
insertBST x (BTBranch r lchild rchild)
  | x <= r = BTBranch r (insertBST x lchild) rchild
  | otherwise = BTBranch r lchild (insertBST x rchild)

-- Given a list l of elements that can be ordered and a binary tree t,
-- inserts all the elements of l in t in a BST fashion.
insertListBST :: (Ord a) => [a] -> BinTree a -> BinTree a
insertListBST list btree = foldr insertBST btree (reverse list)

-- BST inorder traversal
inorder :: BinTree a -> [a]
inorder BTNil = []
inorder (BTBranch r lchild rchild) = inorder lchild ++ [r] ++ inorder rchild


-- Geometry

type Coord = (Double, Double)

type LineSegment = (Coord, Coord)

data Polygon = Triangle Coord Coord Coord
             | Rectangle Coord Coord Coord Coord
             | ConvexPolygon [LineSegment]
             deriving (Show, Eq)

-- Decides if a Polygon is a (valid) triangle
isTriangle :: Polygon -> Bool
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

-- Makes all the possible triangles with the ith vertex taken from the
-- ith argument
makeTriangles :: [Coord] -> [Coord] -> [Coord] -> [Polygon]
makeTriangles xs ys zs = [Triangle x y z | x <- xs, y <- ys, z <- zs, isTriangle $ Triangle x y z]

-- Another (pseudo-)implementation for the previous function
makeTriangles' :: [Coord] -> [Coord] -> [Coord] -> [Polygon]
makeTriangles' [] _ _ = []
makeTriangles' _ [] _ = []
makeTriangles' _ _ [] = []
makeTriangles' (x:xs) (y:ys) (z:zs) = if isTriangle $ Triangle x y z
                                      then makeTriangles' xs ys zs ++ [Triangle x y z]
                                      else makeTriangles' xs ys zs
