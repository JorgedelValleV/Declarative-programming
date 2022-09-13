-- Jorge del Valle Vazquez

getInt::IO Int
getInt = do line <- getLine
            return ( read line::Int)

adivina :: Int -> IO ()
adivina n = 
   do putStrLn "Introduce un numero: "
      x <- getInt
      if (x>n) then (do print ("El valor a adivinar es menor");adivina n) else (if (x<n) then (do print ("El valor a adivinar es mayor");adivina n) else print ("Correcto el numero era " ++ show x))



formatea:: String -> String -> Int -> IO ()
formatea fileIn fileOut n= do 
   i <- readFile fileIn
   let 
    vfil = lines i
    mpal = map words vfil
    res = map concat [esp n fila|fila <- mpal]
    sal = unlines res
    in
     writeFile fileOut sal

-- el 2º arg del zip toma la cantidad de letras por fila
-- mpal es una matriz de palabras 
--entre dos palabras hay un hueco->nºhuecos es nº pal -1
--e sirve para colocar entre las primeras palabras un hueco mas para completar hasta llegar a las columnas deseadas.

esp:: Int->[String]-> [String]
esp n lis = let letras = length $ concat lis in if (letras>n || null lis) then lis
   else
    let
     nhuecos = length lis - 1
     rellenar = n-letras
     coc = rellenar `div` nhuecos 
     res = rellenar `mod` nhuecos 
     esp = replicate coc ' ' 
    in
     [pal++ if res<nh then esp else esp ++ " "| (pal,nh)<- zip lis [1..nhuecos]]++[last lis]


--Matrices
transp:: [[a]]->[[a]]
transp m = if (null $ head m) then [] else (map head m) : transp (map tail m)


sumaLista ::Num a=>[a]->[a]->[a]
sumaLista [] [] = []  
sumaLista (x:xs) (y:ys)
   | length xs  == length ys =(x+y):(sumaLista xs ys)
   | otherwise = error "las listas deben tener igual dim"


sumaMat::Num a => [[a]]->[[a]]->[[a]]
sumaMat [] [] = []
sumaMat (x:xs) (y:ys)  
   | length x == length y && length xs == length ys = (sumaLista x y):(sumaMat xs ys)
   | otherwise = error "las matrices deben tener igual dim"


prodFilMat::(Num a,Eq a) => [a]->[[a]]->[a]
prodFilMat f m = if ((head m) == []) then [] else sum(zipWith (*) f ( map head m)) : (prodFilMat f (map tail  m))
--creo que sirve igual null $ head m

-- producto
prodMat::(Num a,Eq a) => [[a]]->[[a]]->[[a]]
prodMat [] _ = []
prodMat m n
   | length (head m) == length n = (prodFilMat (head m) n): (prodMat (tail m) n)
   | otherwise = error "las matrices deben tener dim adecuada"


dibujaMatriz :: Show a => [[a]] -> IO()
dibujaMatriz m = putStrLn(unlines ( map (unwords . map show) m))



-- otras fromas de hacer algun apartado sin controlar 
-- dimensiones, para dimensiones incorrectas da un resultado
-- erroneo


sumar::Num a => [[a]]->[[a]]->[[a]]
sumar = zipWith (zipWith(+))

prod ::Num a => [a]->[a]->a
prod u v = sum(zipWith (*) u v)

producto ::Num a => [[a]]->[[a]]->[[a]]
producto m n = [ map (prod fila) (transp n) | fila <- m ]

