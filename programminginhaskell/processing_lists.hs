map2 :: (a -> b) -> [a] -> [b]
map2 f xs = [f x | x <- xs]

map3 :: (a -> b) -> [a] -> [b]
map3 f [] = []
map3 f (x:xs) = f x : map3 f xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p xs = [x | x <- xs, p x]

filter3 :: (a -> Bool) -> [a] -> [a]
filter3 p [] = []
filter3 p (x:xs) | p x = x : filter3 p xs
                 | otherwise = filter3 p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

-- takeWhile dropWhile all any 