-- TP Taller de Algebra Primer Cuatrimestre 2015
-- Integrantes:  Ciro Zar, Patricio Lopez Valiente, Franco Nastasi
-- Turno Martes


-- Listas Auxiliares
lLetras = ['a'..'z']

lMasUsadas = [12.52, 1.42, 4.67, 5.85, 13.67, 0.67, 1.01, 0.70, 6.24, 0.44, 0.01, 4.96, 3.15, 6.70, 8.67, 2.51, 0.88, 6.86, 7.97, 4.62, 3.92, 0.90, 0.02, 0.22, 0.90, 0.52]

-- Ejercicio 1

pertenece :: Char -> [Char] -> Bool
pertenece n [] = False
pertenece n (x:xs)
    | n == x = True
    | otherwise = pertenece n xs

esMin :: Char -> Bool
esMin a = pertenece a lLetras

--

nLetra :: Char -> [Char] -> Integer 
nLetra a l1 
    | a == head l1 = 0
    | otherwise = 1 + nLetra a (tail l1)
    
letANat :: Char -> Integer
letANat n = nLetra n lLetras 
										
--

numeL :: Integer -> [Char] -> Char
numeL n l1 
    | n == 0 = head l1
    | otherwise = numeL (n - 1) (tail l1)

natALet :: Integer -> Char
natALet n = numeL n lLetras

--

desplazar :: Integer -> Char -> Char
desplazar n a 
    | n > 25 = undefined 
    | not (esMin a) = a 
    | otherwise = natALet (mod ((letANat a) + n) 26) 

-- 
 
cantMinusc :: String -> Integer
cantMinusc [] = 0
cantMinusc st 
    | esMin (head st) = 1 + cantMinusc (tail st)
    | otherwise = cantMinusc (tail st)
    
--
    
contar :: Char -> String -> Integer
contar a [] = 0
contar a st
    | a == head st = 1 + contar a (tail st)
    | otherwise = contar a (tail st)
    
-- Ejercicio 2

codificar :: Integer -> String -> String
codificar n [] = []  
codificar n st = desplazar n (head st) : codificar n (tail st)

-- Ejercicio 3 

decodificar2 :: Integer -> String -> String
decodificar2 n [] = []  
decodificar2 n st = desplazar (-n) (head st) : decodificar2 n (tail st)

decodificar 0 st = st
decodificar n st 
    | otherwise = codificar (26 - n) st

-- Ejercicio 4

frecLista :: String -> [Char] -> [Float]
frecLista st [] = []  
frecLista st l1 =  (fromIntegral p/fromIntegral q)*100 : frecLista st (tail l1) 
    where p = contar (head l1) st 
	  q = cantMinusc st 


frec :: String -> [Float]
frec st = frecLista st lLetras  

-- Ejercicio 5 

rotar :: Int -> [a] -> [a]
rotar 0 st = st
rotar n st
    | n < 0 = rotar ((length st) + n) st
    | otherwise = rotar (n-1) (tail st ++ [head st])
    
-- Ejercicio 6 

chi2 :: [Float] -> [Float] -> Float
chi2 [] [] = 0
chi2 l1 l2 = ((x1 - y1)^2)/y1 + chi2 xs ys
     where (x1:xs) = l1
	   (y1:ys) = l2

-- Ejercicio 7

listaChi2 :: [Float] -> Int -> [Float]
listaChi2 f n
	| n < 0 = []
	| n >= 0 = chi2 (rotar n f) lMasUsadas : (listaChi2 f (n-1))

chi2convinaciones :: String -> [Float]     
chi2convinaciones st = listaChi2 (frec st) 25

menorChi2 :: String -> Float
menorChi2 st = minimum (chi2convinaciones st)

nRotacion :: Float -> [Float] -> Integer 
nRotacion n l1 
    | n == head l1 = 25
    | otherwise = nRotacion n (tail l1) - 1

descifrar :: String -> String
descifrar st = decodificar (nRotacion (menorChi2 st) (chi2convinaciones st)) st
