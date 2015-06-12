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
--    | ((letANat a) + n) > 25 = natALet ( (letANat a) + n - 26 )
--    | otherwise = natALet ( (letANat a) + n )
    | otherwise = natALet (mod ((letANat a) + n) 26) 
 
 
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

listaChi2 :: String -> [Float] -> Int -> [Float]
listaChi2 st f n
	| n < 0 = []
	| n >= 0 = chi2 (rotar n f) lMasUsadas : (listaChi2 st f (n-1))

minimo :: [Float] -> Float 
minimo (x:xs)
	| xs == [] = x
	| x <= head xs = minimo (x : tail(xs))
	| x > head xs = minimo (xs)

menorChi2 :: String -> Float
menorChi2 st = minimo (listaChi2 st (frec st) 25)

nRotacion :: Float -> [Float] -> Integer 
nRotacion n l1 
    | n == head l1 = 25
    | otherwise = nRotacion n (tail l1) - 1

descifrar :: String -> String
descifrar st = decodificar (nRotacion (menorChi2 st) (listaChi2 st (frec st) 25)) st

-- me puse a mirar el 7) y lo resolvi, pero como vos ya lo subiste 
-- lo dejo aca abajo para que lo veas haber cual queda mejor 

-- !AHORA que me fijo bien, praticamente es lo mismo lo que se nos ocurrio! 
-- pero escrito de otra forma 


dlistas :: [Float] -> Integer -> [Float]

dlistas l1 n -- con l1 = (frec st), n = 26 da todas las convinaciones
    | n == 0 = []
    | otherwise = chi2 l1 lMasUsadas : dlistas (rotar 1 l1) (n-1)

valDeRot :: Float -> [Float] -> Integer

valDeRot m1 ls  -- con m1 = (minimum (dlistas (frec st) 26)), ls = (dlistas (frec st) 26) te da el valor de rotacion  
    | m1 == head ls = 0
    | otherwise = valDeRot m1 (tail ls) + 1
    
descifrar2 :: String -> String 

descifrar2 st = decodificar (valDeRot (minimum (dlistas (frec st) 26)) (dlistas (frec st) 26)) st

lMasUsadas = [12.52, 1.42, 4.67, 5.85, 13.67, 0.67, 1.01, 0.70, 6.24, 0.44, 0.01, 4.96, 3.15, 6.70, 8.67, 2.51, 0.88, 6.86, 7.97, 4.62, 3.92, 0.90, 0.02, 0.22, 0.90, 0.52]
