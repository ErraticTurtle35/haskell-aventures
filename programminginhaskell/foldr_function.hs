sum2 :: Num a => [a] -> a
sum2 = foldr (+) 0

product2 :: Num a => [a] -> a
product2 = foldr (*) 1

or2 :: [Bool] -> Bool
or2 = foldr (||) False

and2 :: [Bool] -> Bool
and2 = foldr (&&) True

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f v []= v
foldr2 f v (x:xs) = f x (foldr2 f v xs)

sum3 :: Num a => [a] -> a
sum3 = foldr2 (+) 0

length2 :: [a] -> Int
length2  [] = 0
length2 (_:xs) = 1 + length2 xs 

length3 :: [a] -> Int
length3 = foldr (\_ n -> 1 + n) 0

reverse2 :: [a] -> [a]
reverse2  [] = []
reverse2 (x:xs) = reverse2 xs ++ [x]