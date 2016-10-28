{- |
Module      :  TypesAndRecursion
Maintainer  :  alorozco.patriot53@gmail.com
Author      :  AlOrozco53
Stability   :  experimental
Portability :  portable
Version     :  0.1

Some examples for the 2017-1 Discrete Structures course.
Taught at Facultad de Ciencias, UNAM.
Topics mainly include natural numbers, lists, and geometry
recursive examples
-}

module TypesAndRecursion where

import PicturesSVG

-- Pictures

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

-- Natural numbers

data Nat = Zero | Succ Nat  deriving (Show, Eq)

sumNat :: Nat -> Nat -> Nat
sumNat Zero m = m
sumNat (Succ n) m = sumNat n (Succ m) 
-- sumNat (Succ n) m = Succ $ sumNat n m

greaterThan :: Nat -> Nat -> Bool
greaterThan Zero _ = False
greaterThan (Succ n) m = case m of
                           Zero -> True
                           Succ y -> greaterThan n y

-- Binary trees

data BinTree a = BTNil | BTBranch a (BinTree a) (BinTree a)  deriving Eq

instance Show a => Show (BinTree a) where
  show btree = showAux btree ""
    where
      showAux BTNil acc = "Nil"
      showAux (BTBranch r lchild rchild) acc = let newacc = acc ++ "\t"
                                               in "Branch " ++ show r ++
                                                  "\n" ++ newacc ++ showAux rchild newacc ++
                                                  "\n" ++ newacc ++ showAux lchild newacc
    
insertBST :: (Ord a) =>  a -> BinTree a -> BinTree a
insertBST x BTNil = BTBranch x BTNil BTNil
insertBST x (BTBranch r lchild rchild)
  | x <= r = BTBranch r (insertBST x lchild) rchild
  | otherwise = BTBranch r lchild (insertBST x rchild)


inorder :: BinTree a -> [a]
inorder BTNil = []
inorder (BTBranch r lchild rchild) = inorder lchild ++ [r] ++ inorder rchild

----------------------------------------------------
----------------------------------------------------
-- DO NOT SHOW THE FOLLOWING CODE TO THE STUDENTS!!!
----------------------------------------------------
----------------------------------------------------

instance Functor BinTree where
  fmap f BTNil = BTNil
  fmap f (BTBranch r lchild rchild) = BTBranch (f r) (fmap f lchild) (fmap f rchild)

insertListBST :: (Ord a) => [a] -> BinTree a -> BinTree a
insertListBST list btree = foldr insertBST btree (reverse list)
