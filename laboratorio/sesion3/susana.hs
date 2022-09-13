milast :: [a] -> a
milast (x:xs) = foldl (\x y->y) x (x:xs)

mireverse :: [a]->[a]
mireverse = foldl (\xs y -> y:xs) []

miall :: (a -> Bool) -> [a] -> Bool
miall = 
