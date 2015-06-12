lLetras = ['a'..'z']


pertenece :: Char -> [Char] -> Bool

pertenece n l1
    | l1 == [] = False   
    | n == head l1 = True
    | otherwise = pertenece n (tail l1)

esMin :: Char -> Bool

esMin a = pertenece a lLetras
 

nLetra :: Char -> [Char] -> Integer 
 
nLetra a l1 
    | a == head l1 = 0
    | otherwise = 1 + nLetra a (tail l1)
    
letANat :: Char -> Integer

letANat n = nLetra n lLetras 


numeL :: Integer -> [Char] -> Char

numeL n l1 
    | n == 0 = head l1
    | otherwise = numeL (n - 1) (tail l1)

natALet :: Integer -> Char

natALet n = numeL n lLetras


desplazar :: Integer -> Char -> Char

desplazar n a 
    | n > 25 = undefined 
    | not (esMin a) = a 
    | ((letANat a) + n) > 25 = natALet ( (letANat a) + n - 26 )
    | otherwise = natALet ( (letANat a) + n )
 
 
cantMinusc :: String -> Integer

cantMinusc st 
    | st == [] = 0
    | esMin (head st) = 1 + cantMinusc (tail st)
    | otherwise = cantMinusc (tail st)
    
    
contar :: Char -> String -> Integer

contar a st
    | st == [] = 0
    | a == head st = 1 + contar a (tail st)
    | otherwise = contar a (tail st)
    
    
codificar :: Integer -> String -> String

codificar n st
    | st == [] = []
    | otherwise =desplazar n (head st) : codificar n (tail st)

    
decodificar :: Integer -> String -> String

decodificar n st = codificar (26 - n) st


frecLista :: String -> [Char] -> [Float]

frecLista st l1  
    | l1 == [] = [] 
    | otherwise =  (fromIntegral p/fromIntegral q)*100 : frecLista st (tail l1) 
    where p = contar (head l1) st 
	  q = cantMinusc st 

frec :: String -> [Float]

frec st = frecLista st lLetras  

rotar :: Int -> [a] -> [a]

rotar n st
    | n == 0 = st
--  | n < 0 = rotar (n+1) (head (reverse st) : (reverse (tail (reverse st)))) esta linea es otra forma de hacer la siguiente, pero mas crota
    | n < 0 = rotar ((length st) + n) st
    | otherwise = rotar (n-1) (tail st ++ [head st])
    
chi2 :: [Float] -> [Float] -> Float

chi2 l1 l2
    | l1 == [] = 0
    | y1 == 0 = 0
    | length l1 == length l2 = ((x1 - y1)^2)/y1 + chi2 xs ys
    | otherwise = undefined
     where (x1:xs) = l1
	   (y1:ys) = l2


lMasUsadas = [12.52, 1.42, 4.67, 5.85, 13.67, 0.67, 1.01, 0.70, 6.24, 0.44, 0.01, 4.96, 3.15, 6.70, 8.67, 2.51, 0.88, 6.86, 7.97, 4.62, 3.92, 0.90, 0.02, 0.22, 0.90, 0.52]
