import Util
import Data.List

--Taller Haskell. PLP Verano 2019

--Código elaborado por Sabrina Castro y Rodolfo Sumoza.

--Algunas pruebas se hicieron en base al autómata mPLP de Util.hs. También se crearon y utilizaron instancias de los autómatas devueltos, y probando los mismos con las funciones sigma y delta. Se probaron autómatas con ciclos finitos e infinitos.


-- Ejercicio 1
vacio :: [b] -> MEN a b
vacio xs = AM xs (\x y -> [])


--La forma de agragar transiciones se hizo aprovechando la transitividad en cada instancia, es decir, la última transición solo el válida para el último automata instanciado. Para esta autómata, se utilizan las funciones de transicion de los anteriores, para completarlo.

agregarTransicion :: (Eq a, Eq b) => MEN a b -> a -> b -> a -> MEN a b
agregarTransicion mi e1 sim e2  | elem e2 (delta mi e1 sim) = AM (sigma mi) (delta mi) 
                                | otherwise = AM (sigma mi) tran 
                                                               where tran e s | e==e1 && s==sim = [e2]++(delta mi e s)
                                                                              | otherwise = (delta mi e s)

aislarEstado :: Eq a => MEN a b -> a -> MEN a b
aislarEstado mi e1 = AM (sigma mi) tran 
                                   where tran e s | e==e1 = []  
                                                  | otherwise = delete e1 (delta mi e s)


-- Ejercicio 2
trampaUniversal :: a -> [b] -> MEN a b
trampaUniversal q ss = AM ss (const $ const [q]) --(\e t ->[q])

completo :: Eq a => MEN a b ->  a  ->  MEN a b
completo mi e1 = AM (sigma mi) (\e t -> (delta mi e t) ++ if elem e1 (delta mi e t) then [] else [e1] )

-- Ejercicio 3
consumir :: Eq a => MEN a b -> a -> [b] -> [a]
consumir mi e xs = foldl f [e] xs 
     where f = \acc x -> concat (map (\y -> delta mi y x) acc ) 

-- Ejercicio 4

--Para establecer la aceptación o no de una palabra, se compara la lista de arroja el consumir una palabra vs. la lista de estados finales, si su interseccion no es nula, se acepta la palabra.

acepta :: Eq a => MEN a b -> a -> [b] -> [a] -> Bool
acepta mi e xs ys =  length (intersect (consumir mi e xs) ys) > 0


-- Para construir el lenguaje se prueban las palabras obtenidas en la funcion Kleene, si se aceptan desde el estado inicial, se incluye en el lenguaje.

lenguaje :: Eq a => MEN a b -> a ->  [a] -> [[b]]
lenguaje mi e xs = filter (\y -> acepta mi e y xs) (kleene (sigma mi))

-- Sugerencia (opcional)
kleene :: [b] -> [[b]]
kleene xs = concat [palabras n xs | n<-[1..] ]


--Palabras devuelve el listado de palabras que tienen una longitud de 1 a n.
palabras :: Integer -> [a]->[[a]]
--palabras 0 _ = [[]]
--palabras n xs = [x++[y] | x<- palabras (n-1) xs, y <- xs ]
palabras n xs = foldNat [[]] (\x -> [ z++[y] | z <-x, y <- xs ]) n


-- Ejercicio 5

-- Esta funcion se basa en recorrer cada rama del árbol, hasta conseguir una hoja, desde ahí, se van contatenando con los avances intermedios. El tipo de la función no fue exactamente el propuesto en el taller.

--trazas :: Eq a => MEN a b -> a -> [[b]]

trazas :: (Eq a, Eq b) => MEN a b -> a -> [[b]]
trazas mi e = map fst (concat (takeWhile (/=[])(iterate (listaIntermedia mi) (listaInicial mi e))))

listaIntermedia :: (Foldable t, Eq t1) => MEN t1 a -> t ([a], t1) -> [([a], t1)]
listaIntermedia mi xs = (foldr (\x acc -> ( [(fst(x)++[y],z) | y <- (sigma mi), delta mi (snd(x)) y /= [], z <- (delta mi (snd(x)) y)] ++ acc  )) [] (xs))

listaInicial :: Eq t => MEN t t1 -> t -> [([t1], t)]
listaInicial mi e = [([x],y) | x<- (sigma mi), delta mi e x /= [], y<-(delta mi e x)]

--Versión recursiva explícita propuesta

--transiciones mi xs 	| (listaIntermedia mi xs) == [] = xs 
--			| True = xs ++ (transiciones mi (listaIntermedia mi xs))

--trazas mi e = map fst (transiciones mi (listaInicial mi e))

--Pruebas construccion de la maquina usando vacia y agregarTransicion
mPLPCons :: MEN Int Char
mPLPCons = agregarTransicion (agregarTransicion (agregarTransicion (agregarTransicion (vacio ['l','p']) 0 'p' 1) 1 'l' 2) 1 'l' 3) 2 'p' 3

mPLPConsCiclo :: MEN Int Char
mPLPConsCiclo = agregarTransicion (agregarTransicion (agregarTransicion (agregarTransicion (agregarTransicion (vacio ['l','p']) 0 'p' 1) 1 'l' 2) 1 'l' 3) 2 'p' 3) 3 'p' 0

mPLPNodoAislado :: MEN Int Char
mPLPNodoAislado = aislarEstado mPLPCons 3


--Ejercicio 6

deltaS :: Eq a => MEN a b -> [a] -> [a]
deltaS = undefined

fixWhile :: (a -> a) -> (a -> Bool) -> a -> a
fixWhile = undefined

fixWhileF :: (a -> a) -> (a -> Bool) -> a -> a
fixWhileF = undefined

alcanzables :: Eq a => MEN a b -> a -> [a]
alcanzables = undefined
