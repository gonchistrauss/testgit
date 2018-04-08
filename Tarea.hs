module Tarea1 where 
	--Nombre: Gonzalo Strauss - 213188
	--Nombre: Martin Rzeszytkowski - 201239

import 	Prelude hiding ((++),reverse)
	
--1(a)
data N = O | S N 
 deriving (Show,Eq,Ord)
--1(b) OPCION 1

-- suma:: N -> N -> N
-- suma = \x y -> case x of{
-- 	O -> y;
-- 	S z -> S (suma z y);
-- }

--1(B) OPCION 2

suma:: N -> N -> N
suma O n = n
suma (S x) n = S (suma x n)

uno = S O
dos = S uno

--Termina cuando con el caso base y es cuando el x es 0. 

-- 1(C)

resta:: N -> N -> N
resta = \x y -> case y of{
	O -> x;
	S z -> case x of{
		S w -> resta w z;
		O -> O;
	};
}

-- Termina con el caso base y es cuando el segundo numero es 0.

-- 1(D)

cociente:: N -> N -> N
cociente m n 
	| m >= n = S (cociente (resta m n) n)
	| otherwise = O

-- Termina con el caso base y es cuando el n es menor al m.

--2(A)

(++) :: [a] -> [a] -> [a]
(++) [] l2 = l2
(++) (x:xs) l2 = x: (xs ++ l2)

--2(B)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

--2(C)
reverseAcum :: [a] -> [a]
acum = []
reverseAcum lista = reverseAux acum lista

reverseAux :: [a] -> [a] -> [a]
reverseAux acum [] = acum
reverseAux acum (x:xs) = reverseAux (x:acum) xs





	


