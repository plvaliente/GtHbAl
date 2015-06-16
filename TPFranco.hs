import Data.Char
--import Data.String

esMin :: Char -> Bool
esMin 'a' = True
esMin 'b' = True
esMin 'c' = True
esMin 'd' = True
esMin 'e' = True
esMin 'f' = True
esMin 'g' = True
esMin 'h' = True
esMin 'i' = True
esMin 'j' = True
esMin 'k' = True
esMin 'l' = True
esMin 'm' = True
esMin 'n' = True
esMin 'o' = True
esMin 'p' = True
esMin 'q' = True
esMin 'r' = True
esMin 's' = True
esMin 't' = True
esMin 'u' = True
esMin 'v' = True
esMin 'w' = True
esMin 'x' = True
esMin 'y' = True
esMin 'z' = True
esMin a = False

letAnat :: Char -> Int
letAnat 'a' = 0
letAnat 'b' = 1
letAnat 'c' = 2
letAnat 'd' = 3
letAnat 'e' = 4
letAnat 'f' = 5
letAnat 'g' = 6
letAnat 'h' = 7
letAnat 'i' = 8
letAnat 'j' = 9
letAnat 'k' = 10
letAnat 'l' = 11
letAnat 'm' = 12
letAnat 'n' = 13
letAnat 'o' = 14
letAnat 'p' = 15
letAnat 'q' = 16
letAnat 'r' = 17
letAnat 's' = 18
letAnat 't' = 19
letAnat 'u' = 20
letAnat 'v' = 21
letAnat 'w' = 22
letAnat 'x' = 23
letAnat 'y' = 24
letAnat 'z' = 25


natAlet :: Int -> Char
natAlet 0 = 'a'
natAlet 1 = 'b'
natAlet 2 = 'c'
natAlet 3 = 'd'
natAlet 4 = 'e'
natAlet 5 = 'f'
natAlet 6 = 'g'
natAlet 7 = 'h'
natAlet 8 = 'i'
natAlet 9 = 'j'
natAlet 10 = 'k'
natAlet 11 = 'l'
natAlet 12 = 'm'
natAlet 13 = 'n'
natAlet 14 = 'o'
natAlet 15 = 'p'
natAlet 16 = 'q'
natAlet 17 = 'r'
natAlet 18 = 's'
natAlet 19 = 't'
natAlet 20 = 'u'
natAlet 21 = 'v'
natAlet 22 = 'w'
natAlet 23 = 'x'
natAlet 24 = 'y'
natAlet 25 = 'z'

desplazar :: Int -> Char -> Char
desplazar n a | esMin a = natAlet(mod (letAnat a + n) 26)
desplazar n a = a

cantMinusc :: String -> Integer
cantMinusc [] = 0
cantMinusc a | esMin (head a) = 1 + cantMinusc (tail a)  
cantMinusc a = cantMinusc (tail a) 

-- función auxiliar igual a cantMinusc
cantMinusc2 :: String -> Float
cantMinusc2 [] = 0
cantMinusc2 a | esMin (head a) = 1 + cantMinusc2 (tail a)  
cantMinusc2 a = cantMinusc2 (tail a) 


contar :: Char -> String -> Integer
contar a [] = 0
contar a b | a == (head b) = 1 + contar a (tail b)
contar a b = contar a (tail b)


--función auxiliar igual a contar pero que devuelve un número de tipo float (para usarlo en la función frec)
contar2 :: Char -> String -> Float
contar2 a [] = 0
contar2 a b | a == (head b) = 1 + contar2 a (tail b)
contar2 a b = contar2 a (tail b)



codificar :: Int -> String -> String
codificar n [] = []
codificar n s = [desplazar n (head s)] ++   codificar n (tail s)

decodificar:: Int -> String ->String
decodificar n [] = []
decodificar n s = [desplazar (-n) (head s)] ++ decodificar n (tail s)

frec :: String -> [Float]
frec s = [(contar2 'a' s)*100/cantMinusc2 s, (contar2 'b' s)*100/cantMinusc2 s, (contar2 'c' s)*100/cantMinusc2 s, (contar2 'd' s)*100/cantMinusc2 s, (contar2 'e' s)*100/cantMinusc2 s,(contar2 'f' s)*100/cantMinusc2 s,
            (contar2 'g' s)*100/cantMinusc2 s, (contar2 'h' s)*100/cantMinusc2 s, (contar2 'i' s)*100/cantMinusc2 s, (contar2 'j' s)*100/cantMinusc2 s, (contar2 'k' s)*100/cantMinusc2 s,(contar2 'l' s)*100/cantMinusc2 s,
            (contar2 'm' s)*100/cantMinusc2 s, (contar2 'n' s)*100/cantMinusc2 s, (contar2 'o' s)*100/cantMinusc2 s, (contar2 'p' s)*100/cantMinusc2 s, (contar2 'q' s)*100/cantMinusc2 s,(contar2 'r' s)*100/cantMinusc2 s,
            (contar2 's' s)*100/cantMinusc2 s, (contar2 't' s)*100/cantMinusc2 s, (contar2 'u' s)*100/cantMinusc2 s, (contar2 'v' s)*100/cantMinusc2 s, (contar2 'w' s)*100/cantMinusc2 s, (contar2 'x' s)*100/cantMinusc2 s,
            (contar2 'y' s)*100/cantMinusc2 s,(contar2 'z' s)*100/cantMinusc2 s] 


--Función auxiliar para el ejercicio 5
rotarEnUno :: [a] -> [a]
rotarEnUno l = tail l ++ [head l]
 
rotar::Integer ->[a] ->[a]
rotar 0 l = l
rotar n l = rotar (n-1) (rotarEnUno l)

chi2::[Float]-> [Float] -> Float
chi2 [] [] = 0
chi2 x y = ((head x - head y)^2)/head y + chi2 (tail x) (tail y)


--Funciones auxiliar para el punto 7:

--Función auxiliar que hace lo que dice el punto 7) b (siempre se tiene que setear el entero a cero):

difPosibRot:: String ->[Float] -> Int -> [Float]
difPosibRot s l 26 = []
difPosibRot s l n = [chi2 l [12.52,1.42,4.67,5.85,13.67,0.67,1.01,0.70,6.24,0.44,0.01,4.96,
                    3.15,6.7,8.67,2.51,0.88,6.86,7.97,4.62,3.92,0.90,0.02,0.22,0.90,0.52] ] 
                    ++ difPosibRot s (rotarEnUno l) (n+1)
                            
--
--Funciones auxiliares para devolver la posición del mínimo de una lista

minLista::[Float] -> Float
minLista l | length l == 1 = head l
minLista l | head l < head(tail l) = minLista([head l] ++ tail(tail l))
minLista l = minLista (tail l) 

-- El entero que entra como argumento se setea a 1
--La forma que se va a usar es : en el primer Float poner el minimo de la lista y en float la Lista
posMinLista:: Float -> Int ->[Float] -> Int
posMinLista _ n l | n > length l = -1
posMinLista f n l | f == head l = n
posMinLista f n l | f  /= head l = posMinLista f (n+1) (tail l)


descifrar :: String -> String
descifrar s = decodificar ((posMinLista (minLista (difPosibRot s (frec s) 0)) 1 (difPosibRot s (frec s) 0 )) -1) s 
