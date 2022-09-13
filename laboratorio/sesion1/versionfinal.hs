--Ejercicio 1
anos :: Float 
anos = 10^6 /(60*60*24*365)

ano :: Int
ano = 10^6 `div`(60*60*24*365)
dia:: Int
dia = 10^6 `mod`(60*60*24*365)`div`(60*60*24)
hor :: Int
hor = 10^6 `mod`(60*60*24*365)`mod`(60*60*24)`div`(60*60)
minu :: Int
minu = 10^6 `mod`(60*60*24*365)`mod`(60*60*24)`mod`(60*60)`div`(60)
seg :: Int
seg = 10^6 `mod`(60*60*24*365)`mod`(60*60*24)`mod`(60*60)`mod`(60)

calcular :: Float -> Float 
calcular n = n /(60*60*24*365)

calc :: Int -> [Char]
calc n = let (ano, dia, hor, min, seg)=(n `div`(60*60*24*365),n `mod`(60*60*24*365)`div`(60*60*24),n `mod`(60*60*24*365)`mod`(60*60*24)`div`(60*60),n `mod`(60*60*24*365)`mod`(60*60*24)`div`(60*60),n `mod`(60*60*24*365)`mod`(60*60*24)`mod`(60*60)`mod`(60))
    in show ano ++ ", " ++ show dia ++ ", " ++ show hor ++ ", " ++ show min ++ ", " ++ show seg

--si necesitamos ver si un ano es bisiesto
esbisiesto ::Int->Bool
esbisiesto ano =(((mod ano 4 == 0) && (mod ano 100 /= 0)) ||(mod ano 400 == 0))


--Ejercicio 2
mediaentera xs    = sum xs `div` length xs

media :: [Int] -> Float
media [] = 0
media l = fromIntegral(sum l) / fromIntegral(length l)


--Ejercicio 3
numDigitos :: Int -> Int
numDigitos x = if(x<10) then 1 else (1 + numDigitos(x `div` 10))


sumDig x = if(x<10) then x else (x `mod` 10 + sumDig(x `div` 10))
reduccion :: Int -> Int 
reduccion x | (x < 0 ) = reduccion (-x)
            | (x < 10 ) = x
            | (x >= 10 ) = reduccion (sumDig x)

--factorial n = product [1..n]
comb :: Int -> Int -> Int 
comb n m | (n<m) = 0
         | (n>=m) = product [1..n] `div`(product [1..m]*product [1..n-m])

--Ejercicio 4
conj :: Bool -> Bool -> Bool
conj False _ = False
conj True y = y

--Ejercicio 5
{-
Mucho :3,7,11,14,15,19
Regular:8,12,13,17,18
Poco:1,2,4,6,9,10,16
Error:5
-}
