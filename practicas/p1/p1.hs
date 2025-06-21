-- ej2
-- i
-- curry :: ((a, b) -> c) -> (a -> b -> c)
-- curry f (x, y) = f x y

-- -- ii
-- uncurry :: (a -> b -> c) -> ((a, b) -> c) 
-- uncurry f = \(x, y) -> f x y

-- iii
-- curryN 1 f  = curry f 
-- curryN n f = f curryN (n-1) f, con n la cantidad de param
-- pero no se puede. La firma tendría una pinta
-- curry :: ((a,b ...) -> c) -> (a -> ... -> c)

---- Esquemas de recursión
-- ej 3
--i 
suma :: [Int] -> Int
suma = foldr (\ x rec -> x + rec) 0 

elemFold :: (Eq a) => a -> [a] -> Bool
elemFold e = foldr (\x rec ->  x == e || rec) False 

ppFold :: [t] -> [t] -> [t]
ppFold xs ys = foldr (\x rec -> x:rec) ys xs

filterF :: (a -> Bool) -> [a] -> [a]
filterF p = foldr (\x rec -> if p x then x:rec else rec) [] 


{-
mapFold f [] = []
mapFold f (x:xs) = f x : mapFold f xs

foldr f casobase lista
-}
mapFold :: (a -> b) -> [a] -> [b]
mapFold f  = foldr (\x rec -> f x : rec) [] 

--ii
{-
devuelve el máximo elemento de la lista según una función de comparación, utilizando foldr1. 
Por ejemplo, maximum = mejorSegún
(>).

Yo quisiera
mejorSegun (>) [x] = x
mejorSegun (>) (x:y:xs) = if x > y then mejorSegun (>) x:xs 
                                    else mejorSegun (>) y:xs

foldr1 tiene como default el ultimo elemento de la lista, entonces no lo escribimos

ej // [3,1,4,2] <-> (3:(1:(4:(2:[])))) <->  
    :                     >
  3   :                 3   >
    1   :        <->      1   >
      4   :                  4  > 2 
         2 []
 
-}
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun op = foldr1 (\x rec-> if op x rec then x else rec) 

-- o sea la acumulada

{-
sumpasP [x] = [x]
sumasP [x, y] = [x, x+y]
sumasP (x:y:xs) = (x+y):sumasP xs

Recordemos que foldl actua asi @ [a,b,c] z = ( (z @ a) @ b ) @ c <- por esto es que se rompe
                                                                  - para listas infinitas

-}

sumasParciales :: (Num a) => [a] -> [a]
sumasParciales = foldl (\rec x -> if null rec then [x] else rec ++ [last rec + x]) []


{-
[a,b,c] = a-b+c

falopa

(-) [3, 1, 4, 2] 0 = 3 - (1 - (4 - (2 - 0)) =
                     3 - (1 - (4 - 2) = 3 - (1 - 4 + 2) = 3 - 1 + 4 -2
-}

sumaAlt :: (Num a) => [a] -> a
sumaAlt = foldr (-) 0

-- no sirve (-) [3, 1, 4, 2] 0
-- ((0-2) -4 -1 -3 ) hace eso
-- sumaAltRev :: (Num a) => [a] -> a
-- sumaAltRev = foldl (-) 0

sumaAltRev :: (Num a) => [a] -> a
sumaAltRev ls = sumaAlt (reverse ls)

-- ej 5 ¿Es o no r. estructural?


{-
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                                    then [x]
                                    else x : elementosEnPosicionesPares (tail xs)

No es r. estructural porque estamos accediendo al resto de la estructura para
saber que hacer, es r primitiva

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) (y:ys) = \ys -> if null ys
                            then x : entrelazar xs [] -> esto es estructural si fuera solo (x:xs) no?
                            else x : head ys : entrelazar xs (tail ys) 

entrelazar no es estructural tampoco pero ??? 
-}

-- ej 6

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b 
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

-- a
sacarUna :: Eq a => a -> [a] -> [a]
{-
sacarUna elem [] = []
sacarUna elem (x:xs) = if (elem == x) then xs else x:sacarUna elem xs

cortamos la recursion ni bien encontramos el primer elemento -}
sacarUna elem = recr (\ x xs rec -> if (elem == x) then xs else x:rec ) [] 

-- b
{-
El problema de foldr es que al ser recursión estructural, no puedo acceder al resto de la subestructura 
durante la llamada recursiva, en cambio con la primitiva si puedo y terminar la recursión.
-}

-- c

{-
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado elem (x:xs) = if (x <= elem) then x:insertarOrdenado elem xs else elem:x:xs

recr f z (x:xs) = f x xs (recr f z xs)
-}
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado elem = recr (\ x xs res -> if (x >= elem) then elem:x:xs else x:res) [elem] 

-- Ej 7
--abersientendi
--rarisimo, copiado del repo de valen
--o sea basicamente no entendi
mapPares :: (a -> b-> c) -> [(a, b)] -> [c] 
mapPares f  = map (\x -> f (fst x) (snd x)) 


-------- otras estructuras

-- ej 9

-- no compro para nada por que es (integer -> b -> b)
-- devuelve un b si, por que recibe un integer y un b?
foldNat :: b -> (Integer -> b -> b) -> Integer -> b
foldNat zero fNoNulo 0 = zero
foldNat zero fNoNulo n = fNoNulo n (foldNat zero fNoNulo (n - 1))

potencia :: Integer -> Integer -> Integer
potencia base = foldNat 1 (\n rec -> base * rec)

-- ej 12

--data AB a = Nil | Bin (AB a) a (AB a)

-- i 
-- llamamos sobre cada rama al caso recursivo respetando su firma
-- aca vemos como tenemos acceso solo sobre n en la recursion
-- foldAB :: b -> (b -> a -> b -> b) -> (AB a) -> b
-- foldAB cBase cRec Nil = cBase
-- foldAB cBase cRec (Bin izq n der) = cRec (foldAB cBase cRec izq) n (foldAB cBase cRec der)

-- -- aca se hace explicito que podemos acceder a las subestructuras no solo en la recursion
-- -- caseo Base
-- -- arbol con "pattern matching" y las dos llamadas recursivas a las subestructurutas, devuelve un b
-- recAB :: b -> (AB a -> a -> AB a -> b -> b -> b) -> (AB a) -> b
-- recAB cBase cRec Nil = cBase
-- recAb cBase cRec (Bin izq n der) = cRec izq n der (recAB cBase cRec izq) (recAB cBase cRec der)


-- de un parcial
data ABE a = Hoja a | Bin (ABE a) a (ABE a)

foldrAEB :: (a -> b) -> (b -> a -> b -> b) -> (ABE a) -> b
foldrAEB fHoja fBin arbol = case arbol of
          Hoja x -> fHoja x
          Bin i n d -> fBin (rec i) n (rec d)
        where rec = foldrAEB fHoja fBin 


recrABE :: (a->b) -> ( (ABE a) -> a -> (ABE a) -> b -> b -> b) -> (ABE a) -> b
recrABE rHoja rBin t = case t of
  Hoja n -> rHoja n
  Bin i n d -> rBin i n d (rec i) (rec d)
  where rec = recrABE rHoja rBin

type SearchTree a = ABE a
  
ej = Bin (Hoja 1) 2 (Hoja 3)

elemSearchTree ::Ord a => a -> SearchTree a -> Bool
elemSearchTree elem = foldrAEB (== elem) (\ ri n rd -> elem == n || if ( elem > n ) then rd else ri) 


-- esNil :: (AB a) -> Bool
-- esNil arbol =  case arbol of
--   Nil -> True
--   _ -> False


-- ej 15