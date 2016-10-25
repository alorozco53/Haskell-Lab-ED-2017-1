# Ejemplos vistos en las tres primeras sesiones de Haskell

***Estructuras Discretas 2017-1***
***Facultad de Ciencias, UNAM***

## Ejecución
En `bin/` se encuentra el archivo `toyfunctions.hs`, el cual se debe de interpretar
desde dentro de dicha carpeta. Es decir,

```shell
cd bin/

ghci toyfunctions.hs
```

## Descripción

Entre las funciones que contiene `toyfunctions.hs` están:

```haskell
magic :: [String] -> [(String, Int)]
```
*Es la función que se vio el viernes 21 de octubre y la cual recibe una lista de cadenas y devuelve*
*una lista con el último carácter de cada una de las cadenas dadas (sufijo de longitud 1) y un contador*
*que indica el de a qué cadena corresponde cada sufijo.*

Ejemplo:
```shell
*ToyFunctions> magic ["The","quick","brown","fox","jumps","over","the","lazy","dog"]
[("e",1),("k",2),("n",3),("x",4),("s",5),("r",6),("e",7),("y",8),("g",9)]
```
======

```haskell
magicK :: [String] -> Int -> [(String, Int)]
```
*Es una generalización de la función anterior pero para sufijos de longitud k.*

Ejemplo:
```shell
*ToyFunctions> magicK ["The","quick","brown","fox","jumps","over","the","lazy","dog"] 2
[("he",1),("ck",2),("wn",3),("ox",4),("ps",5),("er",6),("he",7),("zy",8),("og",9)]
```

Obsérvese que en la implementación se utilizó la palabra reservada `where`, por lo que
se trata de un buen ejemplo para ver su sintaxis y semántica. También se usó la función
`zip` que realiza un mapeo 1-a-1 de dos listas. Su firma es la siguiente:
```haskell
zip :: [a] -> [b] -> [(a, b)]
```
[*Aquí*](http://aprendehaskell.es/content/Empezando.html#tuplas) se encuentra un buen tutorial para entender
el uso de la función `zip`.

======

```haskell
split :: String -> Char -> [String]
```
*Ésta es una implementación no robusta de un equivalente a la función `String.split()` de Java.*
*Recibe una cadena y un carácter delimitador y devuelve todas las cadenas delimitadas por*
*dicho carácter. Su comportamiento es *casi* equivalente al de la función [*`words`*](http://www.cse.unsw.edu.au/~en1000/haskell/inbuilt.html#words)*
*que viene ya dentro del Prelude de Haskell.*

Ejemplo:
```shell
*ToyFunctions> words "The quick brown fox jumps over the lazy dog"
["The","quick","brown","fox","jumps","over","the","lazy","dog"]
```

======

```haskell
initials :: String -> String
```
*Recibe una cadena, la cual se puede uno imaginar como el título de un libro o artículo o de*
*alguna institución que está bien escrito (con iniciales en mayúsculas).*
*Devuelve la cadena con dichas iniciales*

Ejemplo:
```shell
*ToyFunctions> initials "Universidad Nacional Autónoma de México"
"UNAM"
```

## Comentarios adicionales
Cabe destacar que es de suma importancia que se indague lo que hacen las funciones `sum`,
`prod` y `filter`.

Con respecto a ésta última, implementé un *poderoso* **mapeo** para imágenes como las que hemos
trabajado el cual está en la función `picMapping`.