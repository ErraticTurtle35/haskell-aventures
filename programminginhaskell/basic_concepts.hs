add :: Int -> Int -> Int
add x y = x + y

add2 :: Int -> (Int -> Int)
add2 = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)