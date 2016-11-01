# Ejemplos vistos en las sesión del 28 de octubre de 2016 (Haskell)

***Estructuras Discretas 2017-1***

***Facultad de Ciencias, UNAM***

## Ejecución
En `bin/` se encuentra el archivo `datatypes_rec.hs`, el cual se debe de interpretar
desde dentro de dicha carpeta. Es decir,

```shell
cd bin/

ghci datatypes_rec.hs
```

## Descripción

El archivo `datatypes_rec.hs` contiene:

- Números naturales

  - Suma de naturales

    ```haskell
    sumNat :: Nat -> Nat -> Nat
    ```

  - Dados dos números naturales n y m; predicado que decide si n < m.

    ```haskell
    greaterThan :: Nat -> Nat -> Bool
    ```

- Implementación de la función `myDrop`

  - La primera función implementada tiene la siguiente firma:
  
  ```haskell
  myDrop :: Nat -> [a] -> [a]
  ```
  
  Es decir, usa los números naturales en vez de enteros. (Esto para fines de poner
  a **prueba** nuestra implementación de naturales.)
  
- Dos implementaciones de la función `myDropLast`

- Un ejemplo útil con geometría (útiles para la práctica 5)
