module H2048Inicial where

{-
2048 game implementation on console

Based on:
A Haskell implementation of 2048.
Gregor Ulm
https://github.com/gregorulm/h2048
-}

import Prelude hiding (Left, Right)
import Data.Char (toLower)
import Data.List
import System.IO
import System.Random
import Text.Printf

type Grid = [[Int]]

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    grid <- initialGrid
    gameLoop grid

initialGrid :: IO Grid
initialGrid = do
    grid1 <- addTile grid0
    grid2 <- addTile grid1
    return grid2

grid0 :: Grid
grid0 = replicate 4 [0, 0, 0, 0]

addTile :: Grid -> IO Grid
addTile grid = do
    let candidateTiles = getZeroes grid
    pickedTile <- choose candidateTiles
    value  <- choose [2,2,2,2,2,2,2,2,2,4]
    let newGrid = setTile pickedTile value grid
    return newGrid

getZeroes :: Grid -> [(Int, Int)]
getZeroes grid = filter (\(x, y) -> (grid!!x)!!y == 0) positions'

positions' = [(x,y) | x <- [0..3], y <- [0,3]]


--getZeroes' :: Grid -> [Int]
--getZeroes' grid = filter (\(x, y) -> (grid!!x)!!y == 0) [(x,y) | x <- [0..3], y <- [0..3]]

--[(x,y) | x <- [0..3], y <- [0,3]]
--getZeroes' :: Grid -> [(Int, Int)]
--getZeroes' grid = [(x,y) | x <- [0..3], y <- [0,3]]

--getZeroes' :: Grid -> [Int]
--getZeroes' grid = foldr (++) [] grid


--getZeroes'' :: [Int] -> [(Int, Int)]
--getZeroes'' (x:xs) = 
--    do position <- (row, col)
--       nextPosition <- getZeroes'' xs

{-
*H2048> getZeroes [ [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0] ]
[(0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(1,3),(2,0),(2,1),(2,2),
(2,3),(3,0),(3,1),(3,2),(3,3)]

*H2048> getZeroes [ [0,0,2,0], [0,0,0,0], [0,0,0,0], [2,0,0,0] ]
[(0,0),(0,1),(0,3),(1,0),(1,1),(1,2),(1,3),(2,0),(2,1),(2,2),(2,3),
(3,1),(3,2),(3,3)]

*H2048> getZeroes [ [0,0,0,2], [0,0,2,4], [8,16,32,64], [128,256,512,1024] ]
[(0,0),(0,1),(0,2),(1,0),(1,1)]

*H2048> getZeroes [ [2,4,2,4], [4,8,4,8], [2,4,2,4], [16,8,16,8] ]
[]
-}

choose :: [a] -> IO a
choose xs = do
    i <- randomRIO (0, length xs-1)
    return (xs !! i)

setTile :: (Int, Int) -> Int -> Grid -> Grid
setTile (row, col) val grid = pre ++ [mid] ++ post
    where pre  = take row grid
          mid  = take col (grid!!row) ++ [val] ++ drop (col + 1) (grid!!row)
          post = drop (row + 1) grid
--setTile (row, col) val grid = take row grid ++ [] ++ drop (row+1) grid
--setTile :: (Int, Int) -> Int -> Grid -> Grid
--setTile' (row, col) val grid = do
  --  valuedGrid <- getValuedGrid (foldGrid grid)
  --  return (setTile'' valuedGrid)

--setTile'' :: [(Int,(Int,Int))]-> Int
--setTile'' valuedGrid = head (valuedGrid)
--filter (\(x, y) -> (grid!!x)!!y == 0) positions'

foldGrid :: Grid -> [Int]
foldGrid grid = foldl (++) [] grid

getValuedGrid :: [Int] -> [(Int, (Int, Int))]
getValuedGrid grid = zip grid positions'
{-
*H2048Initial> setTile (0,0) 2 [ [0,0,0,0], [0,0,0,0], [0,0,0,0], [0,0,0,0] ]
[[2,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]

*H2048Initial> setTile (1,2) 4 [ [0,0,0,2], [0,0,0,4], [8,16,32,64],
 [128,256,512,1024] ]
[[0,0,0,2],[0,0,4,4],[8,16,32,64],[128,256,512,1024]]
-}

gameLoop :: Grid -> IO ()
gameLoop grid
    | areMovesPossible grid = do
        printGrid grid
        if check2048 grid
        then putStrLn "You won!"
        else do newGrid <- getNewGrid grid
                if grid /= newGrid
                then do new <- addTile newGrid
                        gameLoop new
                else gameLoop grid
    | otherwise = do
        printGrid grid
        putStrLn "Game over"

data Move = Up | Down | Left | Right

areMovesPossible :: Grid -> Bool
areMovesPossible grid = length (areMovesPossible' grid )> 0

areMovesPossible' :: Grid -> [Bool]
areMovesPossible' grid = filter (==True) ([areLeftMovePossible grid] ++ [areRightMovePossible grid] ++ [areUpMovePossible grid] ++ [areMoveDownPossible grid])

areLeftMovePossible :: Grid -> Bool
areLeftMovePossible grid = length(filter (==0) (foldGrid (map merge grid))) >=1

areRightMovePossible :: Grid -> Bool
areRightMovePossible grid = length(filter (==0) (foldGrid (map (reverse . merge . reverse) grid))) >= 1

areUpMovePossible :: Grid -> Bool
areUpMovePossible grid = length(filter (==0) (foldGrid (transpose (map merge (transpose grid))))) >=1

areMoveDownPossible :: Grid -> Bool
areMoveDownPossible grid = length(filter (==0)(foldGrid (map (reverse . merge . reverse) (transpose grid)))) >=1

{-
*H2048Initial> areMovesPossible grid0
True
*H2048Initial> areMovesPossible   [[2,4,2,4],[4,8,4,8],[2,4,2,4],[4,8,4,8]]  
False
*H2048Initial> areMovesPossible [[2,4,2,4],[8,4,8,4],[2,4,2,4],[4,8,4,8]]
True
-}

printGrid :: Grid -> IO ()
printGrid grid = do
    clearScreen
    mapM_ printRow grid

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[2J\n" -- clears the screen

printRow :: [Int] -> IO ()
printRow = foldr (\r rs -> printf "%5d" r >> rs) (putStr "\n")

check2048 :: Grid -> Bool
check2048 grid = length (filter (==2048) (foldGrid grid)) >= 1

check2048' grid = length (filter (==2048) (foldGrid grid)) >= 1
--length (filter (==2048) [0,0,0,2,0,0,0,4,8,16,32,64,128,256,512,1024]) == 1

{-
*H2048Initial> check2048 [ [0,0,0,2], [0,0,0,4], [8,16,32,64], [128,256,512,1024] ]
False
*H2048Initial> check2048 [ [0,0,0,2], [0,0,0,4], [8,16,32,64], [128,256,512,2048] ]
True
-}

getNewGrid :: Grid -> IO Grid
getNewGrid grid = do
    move <- captureMove
    let newGrid = applyMove move grid
    return newGrid

captureMove :: IO Move
captureMove = do
    inp <- getChar
    case lookup (toLower inp) moves of
        Just move  -> return move
        Nothing -> do putStrLn "Use WASD or CHTN as input"
                      captureMove

moves :: [(Char, Move)]
moves = keys "wasd" ++ keys "chtn"
    where keys chars = zip chars [Up, Left, Down, Right]

applyMove :: Move -> Grid -> Grid
applyMove Left  = map merge
applyMove Right = map (reverse . merge . reverse)
applyMove Up    = transpose . applyMove Left  . transpose
applyMove Down  = transpose . applyMove Right . transpose

merge :: [Int] -> [Int]
merge x = take 4 ((merge' (filter (> 0) x)) ++ repeat 0)

merge' :: [Int] -> [Int]
merge' [] = []
merge' [x] = [x]
merge' (x:xs) = if x == (head xs)
                    then x*2 : merge' (tail xs)
                    else x : merge' xs
-- take 4 ([1,2]++repeat 0)
                    {-
*H2048Initial> merge [0,0,2,4]
[2,4,0,0]
*H2048Initial>  merge [0,2,2,4]
[4,4,0,0]
*H2048Initial> merge [2,2,2,4]
[4,2,4,0]
*H2048Initial> merge [2,2,4,4]
[4,8,0,0]
*H2048Initial> merge [2,4,4,4]
[2,8,4,0]
-}
