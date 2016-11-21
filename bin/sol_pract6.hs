{- |
Module      :  SolPract6
Maintainer  :  alorozco.patriot53@gmail.com
Author      :  AlOrozco53
Stability   :  experimental
Portability :  portable
Version     :  0.1

-}

module SolPract6 where

-- Números naturales

data Nat = Zero | Succ Nat  deriving (Show, Eq)

-- Suma de naturales (auxiliar)
sumNat :: Nat -> Nat -> Nat
sumNat Zero m = m
sumNat (Succ n) m = Succ $ sumNat n m

-- Producto de números naturales
prodNat :: Nat -> Nat -> Nat
prodNat Zero _ = Zero
prodNat (Succ n) m = sumNat (prodNat n m) m

-- Calcula el número natural resultante de elevar el primer argumento
-- a la potencia del segundo argumento
powerNat :: Nat -> Nat -> Nat
powerNat _ Zero = Succ Zero
powerNat n (Succ m) = prodNat (powerNat n m) n

-- Decide si los números pasados como argumentos son iguales
eqNat :: Nat -> Nat -> Bool
eqNat Zero m = case m of
                 Zero -> True
                 Succ _ -> False
eqNat (Succ n) m = case m of
                     Zero -> False
                     Succ y -> eqNat n y

-- Mayor estricto (auxiliar)
greaterThan :: Nat -> Nat -> Bool
greaterThan Zero _ = False
greaterThan (Succ n) m = case m of
                           Zero -> True
                           Succ y -> greaterThan n y

-- Decide si el primer argumento es mayor o igual que el segundo argumento
geqNat :: Nat -> Nat -> Bool
geqNat n m = (greaterThan n m) || (eqNat n m)

-- Convierte el Nat pasado a Int
natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

-- Convierte el Int pasado a Nat
intToNat :: Int -> Nat
intToNat n
  | n < 0 = error "fuck off!"
  | n == 0 = Zero
  | otherwise = Succ $ intToNat $ n-1


-- Árboles binarios

data BinTree a = BTNil
               | BTBranch a (BinTree a) (BinTree a)
               deriving Eq

-- Hace un árbol binario instancia de la clase Show de manera conveniente
instance Show a => Show (BinTree a) where
  show btree = showAux btree ""
    where
      showAux BTNil acc = "Nil"
      showAux (BTBranch r lchild rchild) acc = let newacc = acc ++ "\t"
                                               in "Branch " ++ show r ++
                                                  "\n" ++ newacc ++ showAux rchild newacc ++
                                                  "\n" ++ newacc ++ showAux lchild newacc

-- Devuelve la lista con todas las hojas del árbol pasado como argumento
leaves :: BinTree a -> [a]
leaves BTNil = []
leaves (BTBranch r BTNil BTNil) = [r]
leaves (BTBranch _ BTNil rchild) = leaves rchild
leaves (BTBranch _ lchild BTNil) = leaves lchild
leaves (BTBranch _ lchild rchild) = (leaves lchild) ++ (leaves rchild)

-- Cuenta el número de hojas en el árbol dado
nLeaves :: BinTree a -> Nat
nLeaves tree = intToNat $ length $ leaves tree

-- Cuenta el número total de nodos (ramas y hojas) del árbol
nNodes :: BinTree a -> Nat
nNodes BTNil = Zero
nNodes (BTBranch _ lchild rchild) = Succ $ sumNat (nNodes lchild) (nNodes rchild)

-- Cuenta el número de nodos internos (ramas) del árbol
-- SIN hacer recursión directamente
nBranches :: BinTree a -> Nat
nBranches tree = intToNat $ n - l
  where
    n = natToInt $ nNodes tree
    l = natToInt $ nLeaves tree

-- Recorrido preorder
preorder :: BinTree a -> [a]
preorder BTNil = []
preorder (BTBranch r lchild rchild) = [r] ++ preorder lchild ++ preorder rchild

-- Recorrido postorder
postorder :: BinTree a -> [a]
postorder BTNil = []
postorder (BTBranch r lchild rchild) = postorder lchild ++ postorder rchild ++ [r]

-- Auxiliares
inorder :: BinTree a -> [a]
inorder BTNil = []
inorder (BTBranch r lchild rchild) = inorder lchild ++ [r] ++ inorder rchild

insertBST :: (Ord a) =>  a -> BinTree a -> BinTree a
insertBST x BTNil = BTBranch x BTNil BTNil
insertBST x (BTBranch r lchild rchild)
  | x <= r = BTBranch r (insertBST x lchild) rchild
  | otherwise = BTBranch r lchild (insertBST x rchild)

insertListBST :: Ord a => [a] -> BinTree a -> BinTree a
insertListBST [] tree = tree
insertListBST (x:xs) tree = insertListBST xs (insertBST x tree)

-- Ordena la lista dada como argumento
treeSort :: Ord a => [a] -> [a]
treeSort list = inorder $ insertListBST list BTNil

-- [EXTRAS]

-- Busca el primer argumento en el árbol binario de BÚSQUEDA (BST)
-- dado como segundo argumento. La función se evalúa a True syss lo encuentra.
binarySearch :: Ord a => a -> BinTree a -> Bool
binarySearch _ BTNil = False
binarySearch e (BTBranch r lchild rchild)
  | e == r = True
  | e < r = binarySearch e lchild
  | otherwise = binarySearch e rchild

-- Elimina todos los elementos de la lista pasada como segundo argumento
-- que existen en la que se da como primer argumento
(\\) :: Eq a => [a] -> [a] -> [a]
(\\) [] _ = []
(\\) (x:xs) ys = if elem x ys
                 then xs \\ ys
                 else x : (xs \\ ys)

-- Calcula la reversa de una lista
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Dado un árbol binario y un PREDICADO, devuelve la lista de los
-- elementos que cumplen el predicado
ptreeFilter :: BinTree a -> (a -> Bool) -> [a]
ptreeFilter BTNil _ = []
ptreeFilter (BTBranch r lchild rchild) p
  | p r = [r] ++ (ptreeFilter lchild p) ++ (ptreeFilter rchild p)
  | otherwise = (ptreeFilter lchild p) ++ (ptreeFilter rchild p)

-- Hace de Nat una instancia de la clase Ord
instance Ord Nat where
  (<=) n m = geqNat m n
  (<) n m = greaterThan m n
  (>) n m = greaterThan n m
  (>=) n m = geqNat n m
  max n m = if n >= m then n else m
  min n m = if n <= m then n else m

-- Personaje ilustre
famousCScientist :: IO()
famousCScientist =
  do
    putStrLn name
    putStrLn birthdate
    putStrLn contributions
    putStrLn extraInfo
      where
        name = "Alan Turing o Ada Lovelace"
        birthdate = "TBD"
        contributions = "TBD"
        extraInfo = "TBD"
