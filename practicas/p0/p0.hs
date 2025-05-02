
valorAbsoluto :: Float -> Float
valorAbsoluto n | n >= 0 = n
                | otherwise = -n

{-
wikipedia:

Año bisiesto es el divisible entre 4, salvo que sea año secular —último de cada siglo,
terminado en «00»—, en cuyo caso también ha de ser divisible entre 400. 

Por lo tanto, 2004, 2008, 2012, 2016 fueron todos años bisiestos. 
Si bien 2000 fue un año bisiesto, 1900 no lo fue y 2100 tampoco lo será.

-}
bisiesto :: Int -> Bool
bisiesto n | n `mod` 100 /= 0 && n `mod` 4 == 0 = True
           | n `mod` 400 == 0 = True
           | otherwise = False

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1) 


-- Funciones auxiliares
-- Estoy oxidada(?) se puede hacer mas optimo sin lugar a dudas

-- Armo una lista con todos los divisores que tiene el numero n arrancando con d
-- la idea original era que d fuera la raiz de n, pero tuve un problema de tipos
-- que no tenia ganas de arreglar

generoDivisores :: Int -> Int -> [Int]
generoDivisores n 1 = [1]
generoDivisores n d | n `mod` d == 0 = d:generoDivisores n (d-1)
                    | otherwise =  generoDivisores n (d-1)
 
-- agarro un numero, genero sus divisores, si hay mas de 2 (si mismo y el 1), entonces no es primo
esPrimo :: Int -> Bool
esPrimo n | n <= 1 = False
          | otherwise = length (generoDivisores n n) == 2


-- dada la lista de divisores generada para un numero, cuento
-- cuantos primos hay
cantidadDeDivisoresAux :: [Int] -> Int
cantidadDeDivisoresAux [] = 0
cantidadDeDivisoresAux (d:ds) | esPrimo d = 1 + cantidadDeDivisoresAux ds
                              | otherwise = cantidadDeDivisoresAux ds

-- funcion principal
cantidadDeDivisoresPrimos :: Int -> Int
cantidadDeDivisoresPrimos n = cantidadDeDivisoresAux divisores
                       where divisores =  generoDivisores n n

-- ej 3

{-

*Main> inverso (Just 3)
Just 0.3333333333333333

*Main> inverso (Just 0)
Nothing

*Main> inverso (Just (-4))
Just (-0.25)


Agregamos el bound Eq para poder hacer la comparacion de `/= 0`
porque el `Maybe a` no tiene por que tenerlo definido

Idem con Fractional, es para poder hacer 1/n
-}
inverso :: (Eq a, Fractional a) => Maybe a -> Maybe a
inverso (Just n) | n /= 0 = Just (1 / n)
                 | otherwise = Nothing

-- Post volver a leer la consigna, ahí me di cuenta que
-- ya nos daban la firma de la funcion y era mucho más facil jejox
-- Bueno de los errores se aprende

inverso2 :: Float -> Maybe Float
inverso2 n | n /= 0 = Just (1 / n)
           | otherwise = Nothing

{-
Lo primero que intente fue esto, 
la piba no tenia ni idea )?

aEntero :: Either Int Bool -> Int
aEntero r | Right r == False = 0
          | Right r == True = 1
          | otherwise = Left r

despues de 80 años, salio esto
-}

aEntero :: Either Int Bool -> Int
aEntero (Right False) = 0
aEntero (Right True) = 1
aEntero (Left n) = n

-- ej 4

-- borra apariciones de a sobre (x:xs)
borrarApariciones :: Char -> String -> String
borrarApariciones a [] = []
borrarApariciones a (x:xs) | a == x = borrarApariciones a xs
                           | otherwise = x:borrarApariciones a xs  
-- principal
limpiar :: String -> String -> String
limpiar [] orig = orig
limpiar (d:ds) orig = limpiar ds modif
                    where modif = borrarApariciones d orig


suma :: [Float] -> Float
suma [] = 0
suma (x:xs) = x + suma xs

largo :: [Float] -> Float
largo [] = 0
largo (x:xs) = 1 + largo xs

difPromedioAux :: Float -> [Float] -> [Float]
difPromedioAux _ [] = []
difPromedioAux p (x:xs) = cuenta:difPromedioAux p xs
                        where cuenta = x - p

-- principal
difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio lista =  difPromedioAux promedio lista
                    where promedio = suma lista / largo lista

todosIguales :: [Int] -> Bool
todosIguales [x,y] = x == y
todosIguales (x:y:xs) | x /= y = False
                      | otherwise = todosIguales (x:xs)


-- ej 5

data AB a =  Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool 
negacionAB Nil = Nil
negacionAB (Bin izq nodo der) = Bin (negacionAB izq) (not nodo) (negacionAB der)

productoAB :: AB Int -> Int
productoAB Nil = 0
productoAB (Bin Nil nodo Nil) = nodo
productoAB (Bin izq nodo Nil) = nodo * (productoAB izq)
productoAB (Bin Nil nodo der) = nodo * (productoAB der)
productoAB (Bin izq nodo der) = (productoAB izq) * nodo * (productoAB der)