data Prop = Var String | No Prop | Y Prop Prop | O Prop Prop | Imp Prop Prop 


foldProp :: (String -> b) ->  (b->b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Prop ->b
foldProp fvar fno fy fo fimp pr = case pr of
            (Var str) -> (fvar str)
            (No p1) -> fno (rec p1) 
            Y p1 p2 -> fy (rec p1) (rec p2)
            O p1 p2 -> fo (rec p1) (rec p2)
            Imp p1 p2 -> fimp (rec p1) (rec p2)
        where rec = foldProp fvar fno fy fo fimp

variables :: Prop -> [String]
variables op = eliminarRepetidos( foldProp (\s -> [s]) (id) 
                                        (\ri rd -> ri ++ rd)  
                                        (\ri rd -> ri ++ rd) 
                                        (\ri rd -> ri ++ rd) op )

eliminarRepetidos :: Eq a => [a] -> [a]
eliminarRepetidos = foldr (\x xs -> if elem x xs then xs else x:xs) []

prop1 = (O (Var "P") (No (Y (Var "Q") (Var "P"))))

type Valuacion = String -> Bool

evaluar :: Valuacion -> Prop -> Bool
evaluar valuacion = foldProp (\s -> valuacion s) (\p -> not p)
                             (\p1 p2 -> p1 && p2) (\p1 p2 -> p1 || p2)
                             (\p1 p2 -> not p1 || p2)

estaEnFNN :: Prop -> Bool
estaEnFNN = foldProp (\_ -> True) (\n -> case n of
                                      (Var x) -> True
                                      _ -> False)
                    (\p1 p2 -> p1 && p2)
                    (\p1 p2 -> p1 && p2)
                    (\_ _ -> False)
                    