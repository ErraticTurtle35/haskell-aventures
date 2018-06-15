altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap p q [] = []
altMap p q [x] = [p x]
altMap p q (x : y : xs) = p x : q y : altMap p q xs

luhn:: [Int] -> Bool
luhn digits = ((-) (sum(altMap (*2) (*1) digits)) ((length ((filter (>9) (altMap (*2) (*1) digits)))) * 9)) `mod` 10 == 0