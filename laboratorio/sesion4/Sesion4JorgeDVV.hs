-- Jorge del Valle Vazquez
data Direccion = Arriba | Abajo | Izquierda | Derecha deriving (Eq,Ord,Show)
move::Num a => Direccion -> (a,a) -> (a,a)
move Arriba (x,y) = (x,y+1)
move Abajo (x,y) = (x,y-1)
move Izquierda (x,y) = (x-1,y)
move Derecha (x,y) = (x+1,y)

destino::Num a => (a,a) -> [Direccion] -> (a,a)
destino (x,y) [] = (x,y)
destino (x,y) (z:zs) = destino (move z (x,y)) zs

destinofold::Num a => (a,a) -> [Direccion] -> (a,a)
destinofold = foldr move


--b 
data Nat = Cero | Suc Nat deriving (Eq,Ord)

sumita::Nat->Nat->Nat
sumita Cero y = y
sumita (Suc x) y = sumita x (Suc y)
infixr 8 +++
(+++)::Nat->Nat->Nat
(+++) x y = sumita x y

productito::Nat->Nat->Nat
productito Cero y = Cero
productito (Suc x) y = sumita (sumita Cero y) (productito x y )
infixr 8 ***
(***)::Nat->Nat->Nat
(***) x y = productito x y


natToInt::Nat->Int
natToInt Cero = 0
natToInt (Suc x) = 1+ natToInt x

instance Show Nat where show x = show (natToInt x)

--c
data Complejo  = C Float Float deriving (Eq)

sumar::Complejo  ->Complejo  ->Complejo  
sumar (C a b)(C c d) = C(a+ c)( b+ d)

restar::Complejo  ->Complejo  ->Complejo  
restar (C a b)(C c d) = C(a- c)( b- d)

multiplicar::Complejo  ->Complejo  ->Complejo 
multiplicar (C a b)(C c d) = C(a*c-b*d)(a*d+b*c)
instance Num Complejo  where 
    (+) = sumar
    (-) = restar
    (*) = multiplicar

instance Show Complejo  where
    show (C a b) = show a ++ c ++ show b
          where c = if b<0 then "" else "+"
--d

class Medible a where
    medida:: a -> Int

instance Medible Bool where
    medida b = if b then 1 else 0

instance Medible a=> Medible [a] where
    medida [] = 0
    medida (x:xs) = medida x + medida xs

instance (Medible a, Medible b)=>  Medible (a,b) where
    medida (x,y) = medida x - medida y

instance Medible Complejo where
    medida (C a b) = round (sqrt((a^2) +(b^2)))

instance Medible Direccion where
    medida Arriba = 1
    medida Derecha = 1
    medida Abajo = -1
    medida Izquierda = -1

instance Medible Nat where
    medida = natToInt







