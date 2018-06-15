import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
                where weights = iterate (*2) 1

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

encode' :: String -> [Bit]
encode' = addParityBit . concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

decode' :: [Bit] -> String
decode' = map (chr . bin2int . decodeParity) . chop9

transmit :: String -> String
transmit = decode . channel . encode

transmit' :: String -> String
transmit' = decode' . channel . encode'

channel :: [Bit] -> [Bit]
channel = id

evenBit :: [Bit] -> Bool
evemBit [] = True
evenBit bits = even (length  (filter (/=0) bits))

oddBit :: [Bit] -> Bool
oddBit [] = False
oddBit bits = odd (length  (filter (/=0) bits))

addParityBit :: [Bit] -> [Bit]
addParityBit [] = []
addParityBit bits = if oddBit bits
                        then [1] ++ bits
                        else [0] ++ bits

hasParityBit :: [Bit] -> Bool
hasParityBit [] = False
hasParityBit (x:xs) = if oddBit xs && oddBit [x] || evenBit xs && evenBit[x]
                            then True
                            else False

decodeParity :: [Bit] -> [Bit]
decodeParity [] = []
decodeParity (x:xs) = if hasParityBit ([x] ++ xs)
                        then xs
                        else error "There was a problem checking the parity"