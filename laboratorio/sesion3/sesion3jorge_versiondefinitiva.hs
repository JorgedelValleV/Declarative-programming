-- Jorge del Valle Vazquez

--Ejercicio 1

last' :: [a] -> a
last' (x:xs) = foldl (\_ b -> b) x xs
--Lo de error que se hablaba lo planteo asi
--last = foldl (\x y -> y) (error "last: Lista vacia")

reverse' :: [a] -> [a]
reverse' = foldl (\x y -> y : x ) []

all' :: (a -> Bool) -> [a] -> Bool
all' f = foldl (\x y -> f y && x) True

minimum' :: Ord a => [a] -> a
minimum' (x:xs) = foldl (\a b -> if b < a then b else a) x xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\x y -> x ++ [f y]) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldl (\x y -> if f y then x ++ [y] else x ) [] xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' b = foldr (\x y -> if b x then x : y else []) []



--EJERCICIO 2 folds no vacias

foldnvl f [x] = x 
foldnvl f (x:y:xs) = foldnvl f (f x y : xs) 
foldnvr f [x] = x 
foldnvr f (x:xs) = f x (foldnvr f xs) 



--EJERCICIO 3

--a generamos pares (x,-x) y los concatenamos
lista1 = concat [[x,-x] | x<-[1..]]
lista2 = foldr (++) [] [[x,-x] | x<-[1..]]
lista3 = foldnvr (++) [[x,-x] | x<-[1..]]


--b vamos considerando (0,n), (1,n-1),...,(n,0)
listaInf=[(x,n-x)| n<-[0..],x<-[0..n]]
listaInfi = zip (concat [[0..n] | n<-[0..]]) (concat [reverse [0..n] | n<-[0..]]) 
listaInfinita = concat ([zip [0..n] (reverse [0..n])| n<-[0..]])


--EJERCICIO 4

--a
sufijos :: [a] -> [[a]]
sufijos [] = [[]]
sufijos xs = xs : (sufijos $ tail xs)

sufijos' :: [a] -> [[a]]
--sufijos' xs = [drop n xs | n <-[0..], n<= length xs]
sufijos' xs = [drop n xs | n <-[0..length xs]]

--susana
prefijos :: [a] -> [[a]]
prefijos xs = [take n xs | n <-[0..length xs]]

--b
subListas::[a]->[[a]]
subListas xs = [[]]++[take n ys |  n <-[1..length xs],ys<- sufijos xs, n <= length ys]

-susana
sublistas :: Eq a => [a] -> [[a]]
sublistas xs = [] : (filter (/= []) (concat[sufijos ys | ys <- prefijos xs]))

--c
permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones (x:xs) = concat (map (aux [] x) (permutaciones xs))
--obtiene las listas resultantes de insertar el elemento x en una lista, dividiendo la original en dos partes y colocando en medio x      
aux :: [a] -> a -> [a] -> [[a]]
aux xs x [] = [xs ++ [x]]
aux xs x (y:ys) =(xs ++ (x:y:ys)) : (aux (xs ++ [y]) x ys)


--susana
permut:: [a] -> [[a]]
permutaciones [] = [[]]
permut (x:xs) = concat [ intercala x ys | ys <- permut xs]

intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys)) : [ y:zs | zs <-intercala x ys]


--d primera version 
sumandos :: (Num a, Enum a, Ord a) => a -> [[a]]
sumandos 0 = []
sumandos 1 = [[1]]
sumandos n= [a:y:ys| a <- [1..n],(y:ys) <- sumandos (n-a),y >=a] ++[[n]]
