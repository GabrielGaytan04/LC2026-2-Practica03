module Practica03 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"
w = Var "w"
v = Var "v"

{-
FORMAS NORMALES
-}

--Ejercicio 1
fnn :: Prop -> Prop

--ATOMICAS
--Variable atomica. 
fnn (Var p) = Var p 
--Constantes verdaderas y falsas
fnn (Cons True) = Cons True 
fnn (Cons False) = Cons False

--NEGACIONES
fnn (Not (Var p)) = Not (Var p)
--Negacion de la negacion de una variable
fnn (Not (Not (f))) = fnn f
--Negacion de una constante falsa 
fnn (Not (Cons True) ) = (Cons False)
--Negacion de una constante verdadera
fnn (Not (Cons False)) = (Cons True)

--DE MORGAN
--Negacion disyuncion
fnn (Not (Or p q)) = And (fnn (Not p)) (fnn(Not q))
--Negacion conjuncion 
fnn (Not (And p q)) = Or (fnn(Not p)) (fnn (Not q))
--NEGACION DE IMPLICACIONES 
--Negacion Implicacion 
fnn (Not (Impl p q)) = And (fnn p) (fnn(Not q))
--Negacion Bi implicacion 
{-
Para la bicondicional ocupamos algo no tan intuitivo a primera vista.
1. Eliminamos la bicondicional: (p<--->q) = (p--->q) and (q--->p)
2. Trabajamos con una negacion, por cual al tener una conjuncion podemos usar DeMorgan
not((p---->q) and (q---->p)) = (not (p--->q)) or (not (q---->p)). 
-}
fnn (Not (Syss p q)) = Or (fnn (Not (Impl p q))) (fnn (Not (Impl q p)))

--OPERACIONES ENTRE FORMULAS
--a) No necesitamos eliminar conectivo
--Disyuncion
fnn (Or p q) = Or (fnn p)  (fnn q)
--Conjuncion
fnn (And p q) = And (fnn p) (fnn q)
--b) Necesitamos eliminar conectivo 
--Implicacion 
fnn (Impl p q) = Or (fnn(Not p)) (fnn(q))
fnn (Syss p q) = And (fnn(Impl p q)) (fnn(Impl q p))


{-
FUNCION AUXILIAR DISTRIBUCION
Nota: Para el ejercicio 2, tambien nec2esitamos ocupar una funcion auxiliar,en particular para atender el caso 
de las disyunciones de la una formula. Fue complicado entender que solo necesitamos un numero limitado de casos.
Nuestra primera idea fue cubrir todos los casos posibles, pero eso generaba errores de recursion. 
-}

--IMPLEMENTACION
distr :: Prop -> Prop -> Prop
--Primer argumento conjuncion, segundo elemento literal
distr (And formula1 formula2) distribuido = And (distr formula1 distribuido) (distr formula2 distribuido)
--Primer argumento literal, segundo elemento conjuncion 
distr distribuido (And formula1 formula2) = And (distr distribuido formula1) (distr distribuido formula2)  
--Ambas literales. Devolvemos su disyuncion
distr formulaF formulaG = Or formulaF formulaG

{-
FUNCION AUXILIAR DE APLICACION 
Una manera de llegar a la forma normal negativa con mayor facilidad es aplicar una segunda funcion auxiliar 
con la cual podamos llamar a distr y en fnc unicamente llamar a esta funcion. 
-}

--IMPLEMENTACION 
manipulacionFormula :: Prop -> Prop 
--CASO CONJUNCION 
manipulacionFormula (And p q) = And (manipulacionFormula p) (manipulacionFormula q)
--CASO DISYUNCION- Llamamos a la funcion auxiliar de distribucion. 
manipulacionFormula (Or p q) = distr (manipulacionFormula p) (manipulacionFormula q)
--CASO INDIVIDUAL
manipulacionFormula individual = individual

--Ejercicio 2
{-
Finalmente la funcion fnc consta de una instruccion que consta de la llamada a las dos funciones auxiliares
-}
fnc :: Prop -> Prop
fnc f = manipulacionFormula (fnn f)

{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

{-
Para el ejercicio de las clausulas, lo que necesitamos es ocupar nuevamente un par de funciones auxiliares. Una de ellas para descomponer una formula en una lista y otra funcion que genere a la lista 
de listas. 
-}
--FUNCION AUXILIAR PARA PODER DESCOMPONER UNA FORMULA.
clausula :: Prop -> Clausula
--Clausula de una variable atomica
clausula (Var a) = [Var a]
--Clausula de una negacion 
clausula (Not a) = [Not a]
--Disyuncion
{-
NOTA
Se agrego una verificacion para eliminar repetidos de las clausulas. Si la clausula de f1 y de f2 son iguales,
solo agregamos una de ellas, si no, agregamos la concatenacion de clausulas.
-}
clausula (Or f1 f2) = if(clausula f1 == clausula f2)
    then clausula f1
    else clausula f1 ++ clausula f2 

--FUNCION AUXILIAR PARA OBTENER LAS CLAUSULAS DE UNA FORMULA
obtenerClausulas :: Prop -> [Clausula]
obtenerClausulas (And f1 f2) = obtenerClausulas f1 ++ obtenerClausulas f2
obtenerClausulas f = [clausula f]

--Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas f = obtenerClausulas (fnc f)

-- Funcion Auxiliar sinRepetidos reutilizada y modificada un poco,
-- para mantener la clausula como un cjto
sinRepetidos :: [Literal] -> [Literal]
sinRepetidos [] = []
sinRepetidos (x:xs) = if x `elem` xs
                      then sinRepetidos xs
                      else x : sinRepetidos xs

-- Función auxiliar que calcula la literal contraria
contraria :: Literal -> Literal
contraria (Not p) = p
contraria p = Not p
    
--Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion c1 c2 =
  -- Con el if no asumimos que hay resolvente, con esto pasamos el test del truco
  if hayResolvente c1 c2
  then 
    let
      -- Identificamos la literal de c1 que tiene su contraria en c2      
      l = head [x | x <- c1, contraria x `elem` c2]
      lc = contraria l
      -- Quitamos el par de literales complementarias
      c1Limpia = [x | x <- c1, x /= l]
      c2Limpia = [x | x <- c2, x /= lc]
      -- Unimos y con la funcion auxiliar quitamos repetidos
    in
      sinRepetidos (c1Limpia ++ c2Limpia)
  else
    sinRepetidos (c1 ++ c2)

{-
ALGORITMO DE SATURACION
-}

--Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
-- Si la lista no esta vacia -> si hay resolvente
hayResolvente c1 c2 = not (null [l | l <- c1, contraria l `elem` c2])

-- Normaliza una clausula 
normalizarClausula :: Clausula -> Clausula
normalizarClausula = sinRepetidos

-- Elimina clausulas repetidas
sinRepetidosClausulas :: [Clausula] -> [Clausula]
sinRepetidosClausulas [] = []
sinRepetidosClausulas (c:cs)
  | c `elem` cs = sinRepetidosClausulas cs
  | otherwise   = c : sinRepetidosClausulas cs

-- Genera todos los resolventes SIN duplicar pares
resolverTodos :: [Clausula] -> [Clausula]
resolverTodos cs =
  [ normalizarClausula (resolucion c1 c2) |
    (i, c1) <- zip [0..] cs,
    (j, c2) <- zip [0..] cs,
    i < j,                     
    hayResolvente c1 c2
  ]

--Ejercicio 2
--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula.
saturacion :: Prop -> Bool
saturacion f = saturar (map normalizarClausula (clausulas f))
  where
    saturar :: [Clausula] -> Bool
    saturar s =
      let
        nuevos = resolverTodos s
        s' = sinRepetidosClausulas (map normalizarClausula (s ++ nuevos))
      in
        if [] `elem` s'
        then False
        
        else if s' == s
        then True
       
        else saturar s'