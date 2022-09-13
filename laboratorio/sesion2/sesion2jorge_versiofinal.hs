-- Jorge del Valle Vazquez
--Ejercicio1
--Lista cuadrados de 1 a n
apa:: Int -> [Int]
apa 0 = [0]
apa n = apa(n-1) ++ [n^2]

--pares de valor y su cuadrado en orden inverso de 1 a n
apb:: Int -> [(Int,Int)]
apb 0 = [(0,0)]
apb n = [(n,n^2)] ++ apb(n-1)

--sumatorio de una formula i*|cos i |
apc:: Float ->Float
apc 0 = 0
apc n = n*abs(cos(n))+ apc(n-1)

--suma de los multiplos en conjunto de 3 y 5 menores que n
apd::Int ->Int
apd 0 = 0
apd n = apd(n-1)+ (if(n `mod` 3 == 0 || n `mod` 5 == 0) then n else 0)

-- Ejercicio 2  
lista1 :: (Floating b, Enum b) => b -> [b]
lista1 n = map (**2) [0..n]

lista2 :: (Enum b, Floating b) => b -> [(b, b)]
lista2 n = reverse (zip ([0..n]) (map (**2) [0..n]))

lista3 :: (Enum a, Floating a) => a -> a
lista3 n = sum [x*abs(cos(x)) | x <- [1..n]]

lista4 :: Integral a => a -> a
lista4 n = sum [ x | x <- [1..n], (x `mod` 3 == 0 || x `mod` 5 == 0)]

--Ejercicio 3

iguales :: (Eq b, Enum a) => (a -> b) -> (a -> b) -> a -> a -> Bool
iguales f g n m = map (f) [n..m] == map (g) [n..m]


menorA :: (Num a, Enum a) => Int -> a -> (a -> Bool) -> a
menorA n m p = head (dropWhile (not.p) (drop (n-1) [1..m]))


mayor :: (Num a, Enum a) => a -> (a -> Bool) -> a
mayor n p = head (dropWhile (not.p) (reverse [1..n]))

--ex :: (Eq a, Enum a) => a -> a -> (a -> Bool) -> Bool
--ex n m p = []/=[x | x<-[n..m], p x]

ex :: Enum a => a -> a -> (a -> Bool) -> Bool
ex n m p = or(map p[n..m])

--Ejercicio 4
filter2:: [a] -> (a -> Bool) -> (a -> Bool) -> ([a],[a])
filter2 xs p q = (filter p xs, filter q xs)

filters:: [a]->([a->Bool])->[[a]]
filters xs [] = []
filters xs (p:ps) = [filter p xs]++ filters xs ps