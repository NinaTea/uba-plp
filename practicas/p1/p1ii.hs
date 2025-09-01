import Data.List (nub)
-- ej2
-- i
curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)
-- ii
uncurry :: (a -> b -> c) -> ((a, b) -> c) 
uncurry f = \(x, y) -> f x y

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

{-
Explicito

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun _ [x] = x
mejorSegun p (x:xs) = if p x rec then x else rec
    where rec = mejorSegun p xs

-}

-- o sea la acumulada

{-
sumpasP [x] = [x]
sumasP [x, y] = [x, x+y]
sumasP (x:y:xs) = (x+y):sumasP xs

Recordemos que foldl actua asi @ [a,b,c] z = ( (z @ a) @ b ) @ c <- por esto es que se rompe
                                                                  - para listas infinitas

-}
-- iii
sumasParciales :: (Num a) => [a] -> [a]
sumasParciales = foldl (\rec x -> if null rec then [x] else rec ++ [last rec + x]) []


{-
[a,b,c] = a-b+c

falopa

(-) [3, 1, 4, 2] 0 = 3 - (1 - (4 - (2 - 0)) =
                     3 - (1 - (4 - 2) = 3 - (1 - 4 + 2) = 3 - 1 + 4 -2
-}

-- iv
sumaAlt :: (Num a) => [a] -> a
sumaAlt = foldr (-) 0

-- no sirve (-) [3, 1, 4, 2] 0
-- ((0-2) -4 -1 -3 ) hace eso
-- sumaAltRev :: (Num a) => [a] -> a
-- sumaAltRev = foldl (-) 0

-- v
sumaAltRev :: (Num a) => [a] -> a
sumaAltRev ls = sumaAlt (reverse ls)

-- ej 4

--i 
{-
 rec: resultado de permutaciones, es decir, son listas de permutaciones
 
 concatMap funcion [a, b, c] -> concatMap arma una nueva lista
                                aplicando funcion sobre a, b y c y 
                                luego concatena los resultados
 
 En este caso concatMap (funcionLambda) [sublistas con permutaciones]
 concatMap agarra una sublista de permutacion, y llama a map que va a armar una permutacion para 
 todas las posiciones i que generó [0..length elemDeRec]
 
 Luego concatMap sigue aplicando map a cada sublista dentro de rec.
 
 Al final, concatena todo.
-}
permutaciones :: [a] -> [[a]]
permutaciones  = foldr (\x rec -> concatMap (\elemDeRec -> map (armoUnaPermutacionI elemDeRec x) [0..length elemDeRec]) 
                                            rec
                       ) 
                       ([[]])
                where armoUnaPermutacionI elemDeRec x = (\i -> take i elemDeRec ++ [x] ++ drop i elemDeRec)
      

-- ii
-- en rec yo ya tengo mi lista de partes
-- basicamente a las listas de partes que ya tengo tengo 2 opciones
-- les agrego x o no les agrego x
-- (map (x:) rec) ++ rec es la concatenacion de estos dos casos 
partes :: [a] -> [[a]]
partes = foldr (\x rec -> (map (x:) rec) ++ rec) [[]]

-- iii
prefijos :: [a] -> [[a]]
prefijos = foldr (\x rec -> [[]] ++ (map (x:) rec)) [[]]

-- iv
-- [1,2,3]
-- 1 [[2,3],[3], []] -> [1,2,3] : [2,3] :[3]:[]
-- 
-- union de prefijos y sufijos y saco los repetidos

sufijos :: [a] -> [[a]]
sufijos = foldr (\x rec -> (x : head rec) : rec) [[]]

sublistas :: Eq a => [a] -> [[a]]
sublistas l =  nub ((prefijos l)++(sufijos l))


-- ej 5 ¿Es o no r. estructural?

{-
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                                    then [x]
                                    else x : elementosEnPosicionesPares (tail xs)


entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) (y:ys) = \ys -> if null ys
                            then x : entrelazar xs [] -> esto es estructural si fuera solo (x:xs) no?
                            else x : head ys : entrelazar xs (tail ys) 


elementosEnPosicionesPares No es estructural porque a pesar de devolver 
el caso vacio con un caso constante (o sea sin usar f), 
usa la estructura xs en el if y luego hace la recursión sobre tail xs.
   Porque en el caso recursivo puede aparecer la aplicación recursiva de la
    función a la cola (f xs) pero no puede aparecer la función f usada de ninguna 
    otra manera, ni puede aparecer la cola xs usada de ninguna otra manera.

En entrelazar es estructural para (x:xs)? Por la def de arriba si 


-}

--ej 6
-- a) 
-- input: elem, lista
-- output: lista sin la primera aparicion de elem

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

-- tiene sentido usar recursion primitiva porque podemos acceder al resto 
-- de la estructura. Con recursion estructural no podriamos chequear que el
-- elemento esté
sacarUna :: Eq a => a -> [a] -> [a]  
sacarUna elem  = recr (\x xs rec -> if x == elem then xs else x:rec) [] 

-- si el elem a insertar es mas chico que el primer elem de la lista
-- entonces lo appendeamos en ese orden.
-- si no es mas chico, entonces deja al primer elemento donde estaba y repeti la accion
-- con el resto de la lista. 
-- Si llegaste al final, es porque el elemento a insertar era el mas grande de todos.
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado elem = recr (\x xs rec -> if elem < x then elem:x:xs else x:rec) [elem] 

-- ej7
-- curry :: ((a, b) -> c) -> (a -> b -> c)
-- curry f (x, y) = f x y

-- uncurry :: (a -> b -> c) -> ((a, b) -> c) 
-- uncurry f = \(x, y) -> f x y

--Pista: aprovechar la currificación y utilizar evaluación parcial. 
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (\x -> f (fst x) (snd x))

-- armarPares
-- input: (x:xs) (y:ys)
-- output: [(x,y)] y |[(x,y)]| = min {|(x:xs)|, |(y:ys)|}

-- armarPares = foldr f z (x:xs) = f x res ys = if null ys then [] else (x, head ys) : foldr f z xs tail ys

--   el caso base es const [] pues cuando termine la recursión me va a quedar >> 
---                                     foldr f (const []) [] ys = const [] ys
--   si pusiera únicamente la lista vacía no tiparía pues [] ys no tiene sentido
--   const p1 p2 = p1. Recibe 2 argumentos y devuelve siempre el 1ero
--   
-- caso en que la recursión termine antes con xs, llego a (x, head ys), por lo que no vuelvo a llamar a res

armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr (\x rec ys -> if null ys then [] else (x, head ys) : rec (tail ys)) (const [])

--recursion estructural sobre (x:xs)
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = foldr (\ x rec ys -> f x (head ys) : rec (tail ys)) (const []) 

-- ej 8
-- suma de matrices celda a celda
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = foldr (\ x rec ys -> mapDoble (+) x (head ys): rec (tail ys)) (const []) 

transponer :: [[Int]] -> [[Int]]
transponer = foldr (\xs rec -> mapDoble (:) xs rec) (repeat []) 

-- ej 9
foldNat :: (Int -> b -> b) -> b -> Int -> b
foldNat _ z 0 = z
foldNat f z n = f n (foldNat f z (n-1))

potenciacion :: Int -> Int -> Int
potenciacion n = foldNat (\x rec -> n * rec) 1 

-- ej 10
-- i
genLista :: a -> (a -> a) -> Integer -> [a]
genLista _ f 0 = []
genLista ini f n = ini : genLista (f ini) f (n-1)

-- ii
desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta desde hasta = genLista desde (+1) (hasta - desde + 1)

-- ej 11
data Polinomio a = X
                    | Cte a
                    | Suma (Polinomio a) (Polinomio a)
                    | Prod (Polinomio a) (Polinomio a)

foldPol :: b -> (a -> b) -> (b -> b -> b ) -> (b -> b -> b) -> (Polinomio a) -> b
foldPol fX fCte fSuma fProd p = case p of 
                        X -> fX 
                        Cte a -> fCte a
                        Suma i d -> fSuma (rec i) (rec d)
                        Prod i d -> fProd (rec i) (rec d)
                        where rec = foldPol fX fCte fSuma fProd

evaluar :: Num a => a -> Polinomio a -> a
evaluar n poli = foldPol n id (+) (*) poli 
                            
-- ej12
data AB a = Nil | Bin (AB a) a (AB a)

-- i
foldAB :: b -> (b -> a -> b -> b) -> (AB a) -> b
foldAB fNil fBin t = case t of
        Nil -> fNil
        Bin i r d -> fBin (rec i) r (rec d)
        where rec = foldAB fNil fBin

recAB :: b -> (b -> a -> b -> (AB a) -> (AB a)-> b) -> (AB a) -> b
recAB rNil rBin t =  case t of
    Nil -> rNil
    Bin i r d -> rBin (rec i) r (rec d) i d 
    where rec = recAB rNil rBin

-- ii
esNil :: (AB a) -> Bool
esNil t = case t of
    Nil -> True
    _ -> False

altura :: (AB a) -> Int
altura = foldAB 0 (\reci r recd -> 1 + (max reci recd)) 

cantNodos :: (AB a) -> Int
cantNodos = foldAB 0 (\reci r recd -> 1 + reci + recd)

-- iii
-- el ej 3 dice que uses foldr1, o sea no hay caso vacio
-- entiendo que necesitamos un foldr1 que asuma no vacio
-- pero si quiero los elems de la estructura para comparar
-- no me conviene estructural, capaz tengo que usar primitiva? 
-- caos y confusion
-- mejorSegún :: (a -> a -> Bool) -> AB a -> a
-- mejorSegún op = foldAB casoBase 
--                         (\reci r recd -> if op )
-- -- iv
-- b -> (b -> a -> b -> (AB a) -> (AB a)-> b) -> (AB a) -> b
esABB :: Ord a => (AB a) -> Bool
esABB = recAB True (\reci r recd (Bin _ ri _) (Bin _ rd _) -> if ((ri < r) && (r < rd))
                                                             then (reci && recd) 
                                                             else False)
-- v 
-- para ii use estructural, era lo mas directo y no necesitaba mas info que la raiz
-- para el iii NS/NC
-- para el iv use primitiva porque necesitaba acceder a los hijos

-- ej 13
-- caso vacio - [[]] no hay ramas
-- caso no vacio - lista de ramas
--i
ramas :: (AB a) -> [[a]]
ramas = foldAB [[]] (\reci r recd -> map (r:) (reci ++ recd))


esHoja :: AB a -> Bool
esHoja (Bin Nil _ Nil) = True
esHoja _ = False

cantHoja :: (AB a) -> Int
cantHoja = recAB 0 (\reci r recd i d -> if ((esNil i) && (esNil d))
                                     then 1 + reci + recd
                                     else reci + recd) 

--ii
-- quiero ver si tienen la misma forma independientemente de sus nodos
-- o sea si hay algo o no hay algo. Seguro quiero recursion primitiva 

der :: AB a -> AB a
der (Bin i r d) = d

izq :: AB a -> AB a
izq (Bin i r d) = i

raiz :: AB a -> a
raiz (Bin _ r _) = r

-- no me importa el contenido, solo que en esqueleto sean iguales
mismaEstructura :: (Eq a) => (AB a) -> (AB a) -> Bool
mismaEstructura = foldAB (\t2 -> esNil t2) 
                         (\reci r recd t2 ->
                                if esNil t2 then False
                                else (reci (izq t2)) && (recd (der t2)))


espejo :: AB a -> AB a
espejo = foldAB Nil (\reci r recd -> Bin recd r reci)

-- ej14
data AIH a = Hoja a | BinH (AIH a) (AIH a)

foldAIH :: (a -> b) -> (b -> b -> b) -> (AIH a) -> b
foldAIH fHoja fBin t = case t of
        Hoja a -> fHoja a
        BinH i d -> fBin (rec i) (rec d)
        where rec = foldAIH fHoja fBin

alturaH :: AIH a -> Int
alturaH = foldAIH (const 1) (\reci recd -> 1 + max reci recd)

tamañoH :: AIH a -> Int
tamañoH = foldAIH (const 1) (\reci recd -> reci + recd)

-- ej15
-- RoseTree, estructura con un único constructor recursivo
-- Son árboles no vacíos, con una cantidad indeterminada de hijos para cada nodo
data RoseTree a = Rose a [RoseTree a]

foldRose :: (a -> [b] -> b) -> (RoseTree a) -> b
foldRose fRose (Rose r rs) = fRose r (map (foldRose fRose) rs) 

{-
dado un RoseTree, devuelve una lista con sus hojas ordenadas de izquierda a derecha,
según su aparición en el RoseTree
-}
hojasRose :: RoseTree a -> [a]
hojasRose = foldRose (\r rec -> if null rec
                                then [r]
                                else concat rec)

{-
distancias, que dado un RoseTree, devuelve las distancias de su raíz a cada una de sus hojas.
-}

distancias :: RoseTree a -> [Int]
distancias rose = map length (ramasRose rose)

ramasRose :: RoseTree a -> [[a]]
ramasRose = foldRose (\r rec -> if null rec
                                then [[r]]
                                else map (r:) (concat rec))

tamañoRose :: RoseTree a -> Int
tamañoRose = foldRose (\_ rec -> 1 + sum rec)

{-dado un RoseTree devuelve su altura, o sea, la cantidad de nodos de la rama mas larga-}
alturaRose :: RoseTree a -> Int
alturaRose = foldRose (\_ rec -> if null rec
                                 then 1
                                 else 1 + maximum rec)


-- ej16
data HashSet a = Hash (a -> Integer) (Integer -> [a])

vacio :: (a -> Integer) -> HashSet a
vacio fHash = Hash fHash (\x -> [])

pertenece :: Eq a => a -> HashSet a -> Bool 
pertenece e (Hash func hasheado) = elem e (hasheado imagen)
        where imagen = func e

agregar :: Eq a => a -> HashSet a -> HashSet a
agregar elem (Hash h f) = if pertenece elem (Hash h f)
                                    then (Hash h f)
                                    else (Hash h (\y -> if (h elem) == y then elem:f (h elem) else f y))

interseccion :: Eq a => HashSet a -> HashSet a -> HashSet a
interseccion (Hash h1 f1) c2 =  (Hash h1 (filter (flip pertenece c2).f1))

-- last agarra el ultimo elemento de una lista
-- init devuelve la lista sin el ultimo elemento
foldr1Guia :: (a -> a -> a) -> [a] -> a
foldr1Guia f xs =  if (null xs) then (error "lista vacia") else (recr (\y ys -> f y) (last xs) (init xs))

-- ej18

paresQueSumanNat :: Int -> [(Int, Int)]
paresQueSumanNat k = [(i, k - i ) | i <- [0..k]]

paresDeNat :: [(Int, Int)]
paresDeNat = [ p | k <- [0..], p <- paresQueSumanNat k]

-- ej19
pitagóricas :: [(Integer, Integer, Integer)]
pitagóricas = [(a, b, c) | a <- [1..], b <-[1..], c <- [1..], a^2 + b^2 == c^2]
{-
No es una definicion util porque tenemos 3 generadores infinitos uno pegado del otro.
Las leyes dicen:
1. Nunca debe usarse m´as de un generador infinito.
2. El generador infinito siempre va a la izquierda de cualquier otro generador.
3. Los generadores infinitos deben usarse ´unicamente para generar infinitas soluciones1
-}

pitagoricas2 :: [(Integer, Integer, Integer)]
pitagoricas2 = [ (a,b,c) | c <- [1..], a <- [1..c], b <- [1..a],  a^2 + b^2 == c^2]

--ej 20
listasQueSuman :: Int -> [[Int]]
listasQueSuman 1 = [[1]]
listasQueSuman n = [ k:xs | k <- [1..n], xs <- listasQueSuman (n-k)]

-- ej 21
listasDeEnterosPositivos :: [[Int]]
listasDeEnterosPositivos = [lista | k <- [1..], lista <- listasQueSuman k]

-- ej 22
genAIHPorAltura :: Int -> [AIH ()]
genAIHPorAltura 1 = [Hoja ()]
genAIHPorAltura n = [BinH izq der | 
                     altIzq <- [1..n-1],
                     altDer <- [1..n-1],
                     max altIzq altDer == n-1,
                     izq <- genAIHPorAltura altIzq,
                     der <- genAIHPorAltura altDer]

todosLosAIH :: [AIH ()]
todosLosAIH = concatMap genAIHPorAltura [1..]

showAIH :: Show a => AIH a -> String
showAIH (Hoja x) = "Hoja " ++ show x
showAIH (BinH izq der) = 
  "BinH\n" ++
  "  |-- " ++ indent (showAIH izq) ++ "\n" ++
  "  `-- " ++ indent (showAIH der)
  where
    indent = unlines . map ("     " ++) . lines

-- main :: IO ()
-- main = mapM_ (putStrLn . showAIH) (take 5 todosLosAIH)

data Operador = Sumar Int| DividirPor Int | Secuencia [Operador]
 
foldOperador :: (Int-> b) -> (Int -> b) -> ([b]-> b) -> Operador -> b
foldOperador fSumar fDividirPor fSecuencia op = case op of
                    Sumar n -> fSumar n
                    DividirPor n -> fDividirPor n
                    Secuencia xs -> fSecuencia (map rec xs)
                        where rec = foldOperador fSumar fDividirPor fSecuencia

falla :: Operador -> Bool
falla op = foldOperador (\_ -> False) (\n -> n == 0) (\xs -> any id xs) op 
                                                      -- (any id == \xs -> id xs)
                                                      -- si alguno es True, devuelve True

  
aplanar :: Operador -> Operador
aplanar = foldOperador (Sumar) (DividirPor) (\xs-> Secuencia (concatMap armoLista xs)) 
                                        where armoLista ls = case ls of 
                                                          Secuencia s -> s
                                                          op -> [op] 

componerTodas :: [a->a] -> (a->a)
componerTodas = foldl (.) (id) 

aplicar :: Operador -> Int -> Maybe Int
aplicar op n = if falla (aplanar op) then Nothing
                                     else Just ((componerTodas (reverse (listaOpAFunc (aplanar op)))) n)

aplicarOp :: Operador -> (Int -> Int)
aplicarOp op = case op of 
                    Sumar x       -> (+) x
                    DividirPor x -> (`div` x)

listaOpAFunc :: Operador -> [Int -> Int]
listaOpAFunc (Secuencia xs) = map aplicarOp xs
