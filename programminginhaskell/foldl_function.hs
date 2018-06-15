sum2 :: Num a => [a] -> a
sum2 = sum2' 0
          where
          sum2' v [] = v
          sum2' v (x:xs) = sum2' (v+x) xs

sum3 :: Num a => [a] -> a
sum3 = foldl (+) 0

product2 :: Num a => [a] -> a
product2 = foldl (*) 1

or2 :: [Bool] -> Bool
or2 = foldl (||) False

and2 :: [Bool] -> Bool
and2 = foldl (&&) True

lenght2 :: [a] -> Int
lenght2 = foldl (\n _ -> n+1) 0

reverse2 :: [a] -> [a]
reverse2 = foldl (\xs x -> x:xs) []