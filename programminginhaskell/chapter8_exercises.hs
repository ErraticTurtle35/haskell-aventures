-- Exercise 1 
data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int 
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

-- Exercise 5
{-
Given the type declaration data Expr = Val Int | Add Expr Expr define a higher-order function folde
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a such that folde f g replaces each Val constructor in an expression by the function f, and each Add constructor by the function g.

Hutton, Graham. Programming in Haskell (p. 109). Cambridge University Press. Kindle Edition. 

: Gabriel, es mejor hacer las preguntas en los foros para que todos tengan la oportunidad de ver las respuestas.

El ejercicio 5 trata de hacer un fold para una estructura de arbol:

data Expr
= Val Int
| Add Expr Expr

Hay que escribir una funcion folde que camine sobre el arbol y reemplace la funcion constructora Val por una funcion f y Add por la funcion g. 
Es un fold sobre arboles como los que explique en clases la primera vez que fui.
En el programa que deje habia un ArbolBinario y su correspondiente fold. La logica es la misma de un foldr. 
Un foldr reemplaza la funcion constructora de listas (:) por un operador binario y la lista vacia [] por una constante.

ej: dada la lista [1,2,3,4] que se reescribiria 1:(2:(3:(4:[]))) en terminos de sus constructores, aplicando un foldr (+) 0 [1,2,3,4] se tendria:

1 : ( 2 : ( 3 : ( 4 : [] ) ) ) ---> 1 + ( 2 + ( 3 + ( 4 + 0 ) ) )

en general un foldr f g entonces:

1 : ( 2 : ( 3 : ( 4 : [] ) ) ) ---> 1 `f` ( 2 `f` ( 3 `f` ( 4 `f` g ) ) )

o en su notacion prefija:

f 1 ( f 2 ( f 3 ( f 4 g)))

Igual en el programa de mi primera visita esta el codigo de la funcion foldr' para estudiar como reemplaza el f y g por los constructores.
-}
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' op e [] = e
foldr' op e (cabeza:cola) = op cabeza (foldr' op e cola)

data ArbolBinario a = Rama (ArbolBinario a) (ArbolBinario a)
                    | Hoja a
                    deriving Show

arbolito = Rama i d
  where i = Rama (Hoja 1) (Hoja 2)
        d = Hoja 4

sumarArbol (Hoja n) = n
sumarArbol (Rama i d) = sumarArbol i + sumarArbol d

foldArbol :: (b -> b -> b) -> (a -> b) -> ArbolBinario a -> b
foldArbol fr fh (Hoja n) = fh n
foldArbol fr fh (Rama i d) = fr (foldArbol fr fh i) (foldArbol fr fh d)



data Expr = Val Int 
          | Add Expr Expr
          deriving Show

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y

exprs1 = Val 1
exprs2 = Add (Val 2) (Val 3)

folde :: (Int -> a ) ->(a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)