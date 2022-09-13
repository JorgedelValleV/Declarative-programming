-- Jorge del Valle Vazquez
-- Practica Final PD 2020-2021

data Rel a = R [(a,a)]
  deriving (Read, Show)

-- Algunas relaciones (r1 y r5 de equivalencia)
r1, r2, r3 ,r4:: Rel Int
r1 = R([(1,1), (2,2), (3,3), (4,4), (1,4), (4,1)])
r2 = R([(1,3), (2,6), (8,9), (3,6)])
r3 = R([(1,2), (2,3), (3,4), (4,3), (3,2), (2,1)])
r4 = R([(1,2), (2,1), (2,3), (3,2), (3,4), (4,3)])
r5 = composicion r3 r4

-- Funcion que usaremos frecuentemente para obtener la lista
-- de los elementos de la relacion
l::Rel a -> [(a,a)]
l (R xs)=[x|x<-xs]

-- Funcion que asegura eiminar elementos repetidos para
-- obtener conjuntos en vez de multiconjuntos
quitarepetidos::Eq a =>[a]->[a]
quitarepetidos xs = foldr elemento [] xs
  where elemento x ys= if elem x ys then ys else x:ys




-- APARTADO 2
-- Ya hemos hecho deriving Read y Show
-- Redefinimos igual como doble inclusion de conjuntos
instance Eq a => Eq (Rel a) where
  s==t = subconjunto s t && subconjunto t s




-- APARTADO 1
-- Primera aproximacion para asegurar que tenemos una relacion
-- 
esRelacion :: Eq a => Rel a -> Bool
esRelacion (R []) = True
-- esRelacion (R [x]) = True
esRelacion (R (x:xs)) 
  | x `elem` xs = False
  | otherwise = esRelacion (R xs)

esRelacionn :: Eq a => Rel a -> Bool
esRelacionn r=  l r == quitarepetidos (l r)



-- APARTADO 3
-- Primero siempre comprobamos que trabajamos una relacion
-- Todas siguen el esquema:
-- if esRelacion r then (NUCLEO*)
--                 else error " No es relacion"
-- Para mayor visibilidad aparece como comentario el nucleo de
-- cada funcion

-- Valores de salida de la relacion binaria 
--   *quitarepetidos [ x | (x,y) <- l r]
dominio:: Eq a => Rel a -> [a]
dominio r = if esRelacion r then quitarepetidos [ x | (x,y) <- l r] else error "No es relacion"

-- Entendiendo como soporte el rango: valores de llegada
--   *quitarepetidos [ y | (x,y) <- l r]
soporte:: Eq a =>  Rel a -> [a]
soporte r = if esRelacion r then quitarepetidos [ y | (x,y) <- l r] else error " No es relacion"

--Entendiendo como soporte el conjunto de valores sobre el que 
-- se define la relacion, union dominio y rango
--   *juntar (dominio r) (soporte r)
valores :: Eq a =>  Rel a -> [a]
valores r= if esRelacion r then juntar (dominio r) (soporte r)
                           else error " No es relacion"
-- funcion auxiliar de union
juntar::Eq a => [a] -> [a]-> [a]
juntar xs ys = [x|x<-xs, notElem x ys]++ys

-- Considerando los elementos del dominio, serviria el rango,
-- vemos que cada elemento se relaciona consigo mismo
--   *and [elem (x,x) (l r) | x <- dominio r] 
reflexiva :: Eq a => Rel a -> Bool
reflexiva r = if esRelacion r then and [elem (x,x) (l r) | x <- dominio r] else error " No es relacion"

-- Vemos que hay bidireccionalidad  en la relacion
--  *and [(y,x) `elem` (l r) | (x,y) <- l r]
simetrica :: Eq a => Rel a -> Bool
simetrica r = if esRelacion r then and [(y,x) `elem` (l r) | (x,y) <- l r]  else error " No es relacion"


-- Funcion que indica contenidos entre relaciones
-- Se usa para definir (==) y comprobar transitividad
--  *and [elem x (l s) | x <- l r]
subconjunto :: Eq a => Rel a -> Rel a -> Bool
subconjunto r s = if esRelacion r && esRelacion s then and [elem x (l s) | x <- l r] else error " No es relacion"

-- Resultado de componer dos relaciones
-- Se usa ademas junto a subconjunto para definir transitiva
--  *[(x,z) | (x,y) <- l r1, (y1,z) <- l r2, y == y1]
-- Quitamos repetidos para asegurar que el resultado sigue
-- siendo una relacion
composicion :: Eq a => Rel a -> Rel a -> Rel a
composicion r1 r2 = if esRelacion r1 && esRelacion r2 then  R (quitarepetidos[(x,z) | (x,y) <- l r1, (y1,z) <- l r2, y == y1]) else error " No es relacion"

-- Comprobamos que siempre que podamos relacionar dos
-- elementos por relaciones intermedias significa que la
-- relacion de los extremos esta en elconjunto de relaciones
-- Para ello vemos que al componerse consigo misma no
-- encontramos elementos fuera de la relacion de partida
--  *subconjunto (composicion r r) r
transitiva :: Eq a => Rel a -> Bool
transitiva r = if esRelacion r then subconjunto (composicion r r) r else error " No es relacion"

-- Debe cumplir las tres propiedades indicadas
--  *reflexiva r && simetrica r && transitiva r
relEquivalencia :: Eq a => Rel a -> Bool
relEquivalencia r = if esRelacion r then reflexiva r && simetrica r && transitiva r else error " No es relacion"


conjCociente :: Eq a => Rel a -> [[a]]
conjCociente r = if relEquivalencia r then representantes (clases r) else error "No es de Equivalencia"

-- Obtiene la clase para cada elemento, es decir, los 
-- elementos con los que se relaciona
-- Para cada lista el primero se relaciona con el resto
-- Suponemos r de equivalencia
-- Posible solucion:
-- clasesbis r = if esEquivalencia then clases r else error
clases :: Eq a => Rel a -> [[a]]
clases r = [[z|(y,z) <- rs,x==y]|x<-dominio r]
  where rs = l r

-- Llamaremos a esta funcion con clases constriudas
-- Lo que haremos sera escoger un representante por clase
-- Para cada clase si hay una igual la descartamos
-- Suponemos r de equivalencia
-- Posible solucion:
-- rbis r = if esEquivalencia then representantes r else error
representantes ::Eq a => [[a]] ->[[a]]
representantes []=[]
representantes (x:xs)= if (or [elem (head x) y | y <- xs])then representantes xs else x:representantes xs


-- Numeros entre 1 y m tienen divisores entre 1 y m si estos
-- son >= n estan entre n y m
generaDiv :: Integral a =>a -> a -> Rel a
generaDiv n m = R [(x,y)| y<-[1..m], x<-[n..m], y `mod` x == 0]

-- Consideramos la lista en 2 dimensiones y tomamos aquellos
-- que cumplen la relacion >=
generaGE :: Ord a => [a] -> Rel a
generaGE xs = R [(x,y)| x<-xs, y <- xs, x>=y]


-- APARATADO 4

-- Funcion que lee un par
getRel::Read a=> IO (a,a)
getRel = do 
   putStr " El elemento: "
   x <- getLine 
   putStr " esta relacionado con: "
   y <- getLine
   return (read x, read y)

-- Para defini introRel 

-- ENTRADA Y SALIDA
-- Suponemos que los elementos son enteros para simplificar
-- Para para introducimos elemento de parada
-- Podríamos haber pedido el numero de valores a insertar


-- Funcion que lee sucesion de pares y construye a partir de
-- ellos una relacion
introRel:: IO (Rel Int)
introRel= do 
   putStrLn " Escribe una relacion:(0,0)para acabar "
   t <- getRel 
   if t == (0,0) then return (R [])
                 else do 
                    s <- introRel
                    return (R (t:(l s)))

-- Funcion que lee una relacion para mostrarla
-- Se apoya en una funcion que muestra una relacion
muestraRel:: IO ()
muestraRel = do
   x <- introRel
   muestraPro x




-- EXPLICACION MUESTRA POR PANTALLA

-- Escribimos los valores del soporte junto a unos guiones
-- que delimitan la tabla por arriba
cabecera:: Show a => [a] -> IO ()
cabecera xs = do
   putStr("  ")
   superior xs
   putStr("   "++ replicate (3*length xs) ('-')++"   \n")

-- Muestra cada uno de los valores del soporte
superior:: Show a => [a] -> IO ()
superior [] = do
   putStr("  \n")
superior (x:xs) = do
   putStr("  "++show x)
   superior xs

-- Muestra primero el encabezado de cada linea y despues,
-- entre las barras que delimitan la tabla marca x alli
-- donde exista la relacion entre el par de la tabla
ponlinea:: (Show a, Eq a)=> a-> [a]->[(a,a)] -> IO ()
ponlinea a xs ys = do
   putStr(show a++" |")
   fila a xs ys

-- final de la linea 
fila:: (Eq a)=> a-> [a]->[(a,a)] -> IO ()
fila a [] ys= do
  putStr("|  \n")
-- Centro de la linea: cruz si a y x estan relacionados en y
fila a (x:xs) ys= do
  putStr(" ")
  if (elem (a,x) ys) then putStr("x") else putStr(" ")
  putStr(" ") 
  fila a xs ys

-- Parte iferior de la tabla
cuerpo:: (Show a, Eq a)=> [a]-> [a]->[(a,a)] -> IO ()
cuerpo [] xs ys= do
   putStr("   "++ replicate (3*length xs) ('-')++"   \n")
--Muestra todo lo que no es la cabecera, filas y inferior
cuerpo (a:as) xs ys= do
   ponlinea a xs ys
   cuerpo as xs ys

--  Funcion que muestra una relacion usando todo lo anterior
muestraPro:: (Show a, Eq a) => Rel a -> IO ()
muestraPro r = do 
   cabecera (valores r)
   cuerpo (valores r)(valores r) (l r)
   if (relEquivalencia r) then do
                          putStrLn "ES DE EQUIVALENCIA"
                          putStrLn "Observa que la tabla es simetrica y la diagonal esta completa"
                        else putStrLn "No es de equivalencia"








--  AQUI SE ACABA LO PEDIDO EN LA PRACTICA
-- DEBAJO HAY ALGUNOS RESULTADOS QUE SURGIERON DURANTE EL 
-- DESARROLLO DE LA PRACTICA PERO QUEDAN FUERA DE LA ENTREGA
-- COMO EL USO DE mapM_ QUE NO HEMOS VISTO EN CLASE O ALGUNA
-- FORMA DE INCORPORAR LAMBDA- EXPRESIONES EN ENTRADA SALIDA














-- OTRAS COSAS QUE SURGIERON EN EL DESARROLLO

-- Funcion que lee sucesion de pares y construye a partir de
-- ellos una lista de pares
-- Podriamos usarla para obtener la lista de elementos y una
-- vez obtenida crear la relacion a partir de ella que es mas
-- eficiente
-- Asi muestraRel haria muestraPro (R x) en vez de solo x
introRellista:: IO ([(Int,Int)])
introRellista= do 
   putStrLn " Escribe una relacion:(0,0)para acabar "
   t <- getRel 
   if t == (0,0) then return []
                 else do 
                    r <- introRellista
                    return (t:r)


-- Alguna prueba usando mapM_
muestraProbis:: (Show a, Eq a) => Rel a -> IO ()
muestraProbis r = do 
   cabecera (valores r)
   mapM_ (\x -> ponlineabis x (valores r) (l r)) (valores r)
   putStr("   "++ replicate (3*length (valores r)) ('-')++"   \n")

ponlineabis:: (Show a, Eq a)=> a-> [a]->[(a,a)] -> IO ()
ponlineabis a xs ys = do
   putStr(show a++" |")
   mapM_ (\x -> do
      putStr(" ")
      if (elem (a,x) ys) then putStr("x") else putStr(" ")
      putStr(" ") ) (xs)
   putStr("|  \n")


-- Para hacer una introRel general deberiamos modificar getRel
getInt:: IO Int
getInt = do 
   x <- getLine 
   return (read x::Int)

intro:: Read a=>IO (Rel a)
intro= do 
   putStr " Numeros a insertar: "
   n <- getInt 
   v <-introRelGeneral n
   return v

introRelGeneral:: Read a =>Int ->IO (Rel a)
introRelGeneral n = do 
   if n==0 then return (R [])
           else do 
              t <- getRel
              s <- introRelGeneral (n-1)
              return (R (t:(l s)))

