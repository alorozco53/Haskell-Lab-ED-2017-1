# Ejemplos vistos en las sesión del 4 de noviembre de 2016 (Haskell)

***Estructuras Discretas 2017-1***

***Facultad de Ciencias, UNAM***

## Ejecución
En `bin/` se encuentra el archivo `btigeom.hs`, el cual se debe de interpretar
desde dentro de dicha carpeta. Es decir,

```shell
cd bin/

ghci btigeom.hs
```

## Descripción

El código fuente está dividido en lo siguiente:

### Listas

Aquí solo se implementó la función `show` que hace instancia al tipo de datos `MyList a` a
la clase `Show` (la cual permite que un tipo sea impreso en la terminal al convertirlo a
`String`).

- 
  ```haskell
  showList1 :: Show a => MyList a -> String
  ```
  Esta función es la que se revisó en clase e imprime listas en el estilo
  `1 :(2 :(3 :[]))`.
  
- 
  ```haskell
  showList2 :: Show a => MyList a -> String
  ```
  Esta función imprime de *mejor* manera a una lista:
  `[1,2,3]`. La estrategia que se usó fue, primero, separar el caso base e inmediatamente
  evaluar a `[]`. Posteriormente, lo primero que se hace es concatenar el `[` con lo que
  resulte de una función auxiliar y, finalmente, pegar el `]` al final. Dicha función auxiliar
  coloca *comas* entre cada elemento de la lista (observe que el **primer** elemento de la lista
  siempre se imprime fuera de la función auxiliar para no tener una coma delante de éste).

Para visualizar lo que hace cada una de las funciones anteriores, basta cambiar el `showList2` a
`showList1` en la declaración 
```haskell
instance Show a => Show (MyList a) where
  show = showList2
```
y construir cualquier `MyList a` en la terminal:
```haskell
*BTInstGeom> Cons 1 (Cons 2 (Cons 3 Nil))
```

### Árboles binarios

- 
  ```haskell
  heightBT :: BinTree a -> Int
  ```
  Nos brinda la altura de un árbol binario. Recordemos que, bajo nuestra definición, la altura de una
  sola _hoja_ es 1 (ya que tiene dos hijos nulos) y la altura de un árbol `BTNil` es 0.
  
- 
  ```haskell
  insertBST :: (Ord a) =>  a -> BinTree a -> BinTree a
  ```
  Inserta un elemento a un árbol binario dado de acuerdo a un (árbol binario de búsqueda)[https://www.cs.usfca.edu/~galles/visualization/BST.html].
  
-
  ```haskell
  insertListBST :: (Ord a) => [a] -> BinTree a -> BinTree a
  ```
  Inserta todos los elementos de la lista dada en el árbol binario dado (BST). Ojo, esta función
  es **un regalo**.
  
- 
  ```haskell
  inorder :: BinTree a -> [a]
  ```
  Realiza un recorrido `inorder`, es decir, regresa la lista *ordenada* de todos los elementos
  en el árbol de búsqueda binaria **dado**. Recordemos que el recorrido `inorder` recorre
  recursivamente, primero, el hijo izquierdo de una rama luego va hacia la raíz y, finalmente,
  recorre recursivamente el hijo derecho.

### Geometría

- 
  ```haskell
  makeTriangles :: [Coord] -> [Coord] -> [Coord] -> [Polygon]
  ```
  Solución elegante al problema dejado en la práctica 5.
  
- 
  ```haskell
  makeTriangles' :: [Coord] -> [Coord] -> [Coord] -> [Polygon]
  ```
  Una *pseudo-*solución al problema, inspirado por algún alumno de la clase. Obsérvese que
  en este caso se producen muchos menos triángulos de los que se deberían de producir.
  Sin embargo, como **no** es trivial llegar a la solución elegante, se considerará positivo
  haber puesto una solución como ésta en la práctica
  
Con respecto a la función `isTriangle`, se explicará a detalle la implementación cuando
se suba la solución a otra rama (REMINDER: put link here).

