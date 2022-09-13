--Jorge del Valle Vázquez
--Ejercicio 1
10^6 `div` (60*60*24*365)

10^6 `div` (60*60*24*365)
10^6 `div` (60*60*24)
10^6 `div` (60*60)
10^6 `div` (60)
10^6 


--Ejercicio2
media xs    = sum xs `div` length xs

media [Int] -> Float
media [] = 0
media l = fromIntegral(sum l) `div` fromIntegral(length l)


--Ejercicio 3
numDigitos :: Int -> Int
numDigitos 0 = 0
numDigitos x | (x `div` 10 == 0) = 1
	     | (x `div` 10 == 1) = 1 + numDigitos (x `div` 10)

sumDig :: Int -> Int
sumDig 0 = 0
sumDig x = x `mod` 10 + reduccion (x `div` 10)
reduccion :: Int -> Int 
reduccion x | (x < 10 ) = sumDig x
	    | (x >= 10 ) = sumDig (x)

--factorial n = product [1..n]
comb :: Int -> Int -> Int 
comb n m | (n<m) = 0
	 | (n>=m) = product [1..n] `div`(product [1..m]*product [1..n-m])


--Ejercicio 4
and x y = x && y

--Ejercicio 5
{-
Mucho :3,7,11,14,15,19
Regular:8,12,13,17,18
Poco:1,2,4,6,9,10,16
Error:5
-}

-- Es bisiesto
esbisiesto ::Int->Bool
esbisiesto año =(((mod año 4 == 0) && (mod año 100 /= 0)) ||(mod año 400 == 0))


