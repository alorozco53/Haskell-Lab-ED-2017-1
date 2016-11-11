{- |
Module      :  HOFRecursion
Maintainer  :  alorozco.patriot53@gmail.com
Author      :  AlOrozco53
Stability   :  experimental
Portability :  portable
Version     :  1.3

Some examples for the 2017-1 Discrete Structures course.
Taught at Facultad de Ciencias, UNAM.
Topics mainly include recursion and higher order functions.
-}

module HOFRecursion where

import PicturesSVG

-- Binary trees

data BinTree a = BTNil
               | BTBranch a (BinTree a) (BinTree a)
               deriving Eq

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
inorder (BTBranch r lchild rchild) =
  inorder lchild ++ [r] ++ inorder rchild


instance Functor BinTree where
  fmap f BTNil = BTNil
  fmap f (BTBranch r lchild rchild) = BTBranch (f r) (fmap f lchild) (fmap f rchild)

insertListBST :: (Ord a) => [a] -> BinTree a -> BinTree a
insertListBST list btree = foldr insertBST btree (reverse list)


-- Propositions

data Prop = VarP String            -- A propositional variable encoded in a String
          | TTrue                  -- T
          | FFalse                 -- ⊥
          | Neg Prop               -- ¬ Φ
          | Disj Prop Prop         -- Φ ^ Ψ
          | Conj Prop Prop         -- Φ v Ψ
          | Imp Prop Prop          -- Φ -> Ψ
          | Equiv Prop Prop        -- Φ <-> Ψ
          deriving Show

-- Tells which variables are considered to be true
type State = [String]

-- Tokenizes a propositional formula in reversed Polish notation
propLexer :: String -> [String]
propLexer [] = []
propLexer (x:xs)
  | x == ' ' = propLexer xs
  | x == '<' = case xs of
                 [] -> error "TOKEN: <, not recognized!"
                 (y:ys) -> if y == '-'
                           then case ys of
                                  [] -> ["<-"]
                                  (z:zs) -> if z == '>'
                                            then "<->" : propLexer zs
                                            else error $ "TOKEN: " ++ "<-" ++ [z] ++ ", not recognized!"
                           else error $ "TOKEN: " ++ "<" ++ [y] ++ ", not recognized!"
  | x == '-' = case xs of
                 [] -> error "TOKEN: -, not recognized!"
                 (y:ys) -> if y == '>'
                           then "->" : propLexer ys
                           else error $ "TOKEN: " ++ "-" ++ [y] ++ ", not recognized!"
  | elem x "FT&¬|" = [x] : propLexer xs
  | otherwise = [varToken] ++ propLexer restString
  where
    varToken = takeWhile (\t -> not $ elem t " <-FT¬&|>") (x:xs)
    restString = case dropWhile (\t -> not $ elem t " <-FT¬&|>") (x:xs) of
                   [] -> []
                   c@(y:ys) -> if y == '>'
                               then error "TOKEN: " ++ c ++ ", not recognized!"
                               else c

-- Builds the parse tree of a list of reversed-Polish tokens
parseTree :: [String] -> BinTree String
parseTree tokens = parseTree' tokens []

parseTree' :: [String] -> [BinTree String] -> BinTree String
parseTree' [] stack = case stack of
                       [] -> error "Syntax error!"
                       (y:ys) -> if null ys
                                 then y
                                 else error "Syntax error!"
parseTree' (x:xs) stack
  | elem x ["T", "F"] = parseTree' xs ((BTBranch x BTNil BTNil) : stack)
  | x == "¬" = case stack of
                 [] -> error "Syntax error!"
                 (y:ys) -> parseTree' xs ((BTBranch x BTNil y) : ys)
  | elem x ["&", "|", "->", "<->"] = case stack of
                                       [] -> error "Syntax error!"
                                       (y:ys) -> case ys of
                                                   [] -> error "Syntax error!"
                                                   (z:zs) -> parseTree' xs ((BTBranch x z y) : zs)
  | otherwise = parseTree' xs ((BTBranch x BTNil BTNil) : stack)

-- Recursion

-- Factorial
fact :: (Ord a, Num a) => a -> a
fact n
  | n <= 0 = 1
  | otherwise = n * (fact $ n-1)


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
