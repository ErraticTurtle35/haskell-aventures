import Data.Char
-- Exercise 1 (I don't understand.. how? and why?)
putStr' :: String -> IO ()
putStr' xs  = do sequence_ [ putChar x | x <- xs ] 

-- Exercise 2
type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

putBoard' :: Int -> Board -> IO ()
putBoard' row [] = return ()
putBoard' row (x:xs) = do putRow row x
                          putBoard' (row+1) xs

-- Exercise 3
putBoard'' :: Board -> IO ()
putBoard'' b = sequence_ [putRow r n | (r,n) <- zip [1..] b]


-- Exercise 4
newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERROR: Invalid digit"
                           getDigit prompt

adder :: Int -> IO ()
adder n =
   do array <- intArray n
      putStrLn (show (sum array))

intArray :: Int -> IO [Int]
intArray 0 = return []
intArray n = 
    do newline
       str <- getLine
       nextInt <- intArray(n-1)
       let int = read str :: Int
       return (int:nextInt)

adder' :: Int -> IO ()
adder' n =
   do array <- intArray' n
      putStrLn (show (sum array))

intArray' :: Int -> IO [Int]
intArray' 0 = return []
intArray' n = 
    do newline
       digit <- getDigit "Insert digit:"
       nextInt <- intArray'(n-1)
       return (digit:nextInt)