import Data.Char (isDigit, toUpper)
import Data.List (intercalate, transpose, maximumBy)
import Data.Function (on)
import Data.Maybe

type FileName = String
type Field = String
type Row = [Field]
type Table = [Row]

salariesFileName = "salarios.txt"
clientFileName = "clientes.txt"
orderFileName = "ordenes.txt"

readDB :: FileName -> IO String
readDB = readFile fileName

processDB :: IO Table
processDB
    = do contents <- readDB salariesFileName
         let contentsLines = lines contents
         let salariesTable = parseTable contentsLines
         return salariesTable

-- Exercise 3.1.1. 
parseTable :: [String] -> Table
parseTable lines = map words lines

-- Exercise 3.1.2. 
printLine' :: [Int] -> String
printLine' [] = ""
printLine' [x]     = "+" ++ replicate x '-' ++ "+"
printLine' (x:xs) =  "+" ++ replicate x '-' ++ printLine' xs

printLine :: [Int] -> String
printLine [] = ""
printLine x = foldr (++) "+" (map (\x -> '+' : replicate x '-') x)

-- Exercise 3.1.3. 
printField :: Int -> String -> String
printField w f = if all isDigit f
                   then replicate (w - length f)  ' ' ++ f
                   else f ++ replicate (w - length f)  ' '

-- Exercise 3.1.4. 
printRow :: [(Int, String)] -> String
printRow [] = ""
printRow rs = "|" ++ (intercalate "|" (map (\r -> (uncurry printField r)) rs)) ++ "|"

-- Exercise 3.1.5. 
test :: IO [Int]
test =
       do table <- processDB
          return (columnWidths table)

columnWidths :: Table -> [Int]
columnWidths [] = []
columnWidths t = map (\r -> length (maximumBy (compare `on` length ) r)) (transpose t)

-- Exercise 3.1.6. 
test' :: IO ()
test' =
       do table <- processDB
          let prettyTable = printTable table
          putStr ( unlines prettyTable )

printTable :: Table -> [String]
printTable [] = []
printTable t = printLine (columnWidths t) : printTableHeader t: printLine (columnWidths t) : printTableBody t ++ [printLine (columnWidths t)]

printTableHeader :: Table -> String
printTableHeader t = (toUpper' (head (map (\r -> printRow (zip (columnWidths t) r)) (t))))

printTableBody :: Table -> [String] 
printTableBody t = (map (\r -> printRow (zip (columnWidths t) r)) (tail t))

toUpper' :: String -> String
toUpper' s =  map (\c -> toUpper c) s

-- Exercise 3.1.7. 
test'' :: IO ()
test'' =
        do table <- processDB
           let filteredTable = select "gender" "male" table
           let prettyTable = printTable filteredTable
           putStr ( unlines prettyTable )

select' :: Field -> Field -> Table -> Table
select' c v (t:ts) = maybe (t:ts) (: (checkRow v ts)) (Just (checkColumn c t))

select :: Field -> Field -> Table -> Table
select c v (t:ts) = if c `elem` t
                      then t : checkRow v ts
                      else t:ts

checkColumn :: Field -> Row -> Row
checkColumn c r  
         |c `elem` r = r
         |otherwise = []

checkRow :: Field -> Table -> Table
checkRow v t = filter (\r -> ((length r) > 0))(map (\r ->  checkColumn v r) t)

-- Exercise 3.1.8. 
test''' :: IO ()
test''' =
        do table <- processDB
           let filteredTable = project ["last", "first", "salary"] table
           let prettyTable = printTable filteredTable
           putStr ( unlines prettyTable )

project :: [Field] -> Table -> Table
project fs t = transpose (foldr (++) [] (map (\f -> checkRow f (transpose t)) fs))

-- Exercise Wrapping up
-- No llegue a comprender por completo como utilizar el interact, lo siento.
exercise :: [Field] -> [Field] -> IO ()
exercise (k:vs) pfs =
                 do table <- processDB
                    let selectedTable = select k (head vs) table
                    let filteredTable = project pfs selectedTable
                    let prettyTable = printTable filteredTable
                    putStr ( unlines prettyTable )

exercise' :: [String] -> IO [String]
exercise' (x:xs) = do table <- processDB
                      let selectedTable = select x (head xs) table
                      let filteredTable = project (tail xs) selectedTable
                      let prettyTable = printTable filteredTable
                      return prettyTable

-- Bonus Exercise (join)
test'''' :: IO ()
test'''' =
        do clientTable <- processDB' clientFileName
           orderTable <- processDB' orderFileName
           let joinedTable = join clientTable orderTable
           let prettyTable = printTable joinedTable
           putStr ( unlines prettyTable )

test''''' :: IO ()
test''''' =
        do clientTable <- processDB' clientFileName
           orderTable <- processDB' orderFileName
           if "customer_id" `elem` (head orderTable)
              then putStr (unlines (printTable (join clientTable orderTable)))
              else putStr ( unlines [" "] )


processDB' :: String -> IO Table
processDB' s
    = do contents <- readDB s
         let contentsLines = lines contents
         let table = parseTable contentsLines
         return table

join :: Table -> Table -> Table
join (t1:t1s) (t2:t2s) = (t1 ++ t2) : (join' t1s t2s)

join' :: Table -> Table -> Table
join' [] _ = []
join' (t1:t1s) t2s = join'' t1 t2s ++ join' t1s t2s

join'' :: Row -> Table -> Table
join'' r ts = map (\t -> r ++ t) (filter  (\t -> ((last t) == (head (r)))) ts)

{- Reflection
Question 3.1.1. 
Assume you were asked to implement this program in an imperative programming language such as C# or Java,
but were not provided with as much guidance on how to structure your program,
nor had made this exercise in a functional language before. How would you have structured your program?

Creo que hubiera escrito mucho más código…
Hubiera creado varias clases entre ellas:
- Unas tres clases para manejar las relaciones entre la Tabla que esta compuesta de Líneas y ella a su vez de Atributos.
- Una clase para manejar el formato de la tabla
- No estoy muy seguro de si crear una clase para el manejo de la lectura de los archivos... porque para el ejemplo no 
se necesita mucho.... por lo menos eso creo ahora. ;)
- Una clase “main” para que sea el inicio de la app.


Question 3.1.2. 
Now that you have made this exercise in a functional programming language, 
do you think that you would implement this program differently in an imperative language than if you had not? 
Especially think about the amount of work a particular function does, the types of the functions,
the use of higher-order functions, and the use of side-effects (System.Console.WriteLine is a side-effecting function!) 

Lo hubiera hecho diferente… con estos ejercicios he podido mejorar mi manejo de colecciones,
en mi trabajo diario utilizo python y he empezado a trabajar usando métodos que ahora se que son heredados de Haskell del paquete “itertools”
Este modulo posee alguno no todas las funciones que posee Haskell.

Question 3.1.3. 
In Exercise 3.1.3 we mentioned that printField should satisfy the property 8n s.n>=length s=>length (printField n s)==n. 
Does it also sat- isfy the property 8n s.length (printField n s)==n?
Would it be a problem in this program if it would not?

Question 3.1.4. 
The property 8n s.n>=length s=>length (printField n s)== n is written in a mixture of predicate logic and Haskell. 
Could you express it as a mixture of predicate logic and C] or Java? 
How would you have phrased this property if you had implemented printField using System.Console.WriteLine? 
If I wrongly claimed that your program does not meet its specification, 
which of the formulations would you prefer to use to argue that you deserve a higher grade?        

Question 3.1.5. 
In the function printTable, how often do you compute the required widths of the fields?

Unas tres veces por cada palabra... creo que es un poco ineficiente con grandes cantidades de datos...


Question 3.1.6. 
While we guaranteed the input database contains at least one column, 
it is actually possible to create an “empty” table with no columns using the project operation 
(We will be nice and not test for this corner case, however.) 
How do you think such an empty table should be represented in both its abstract and its concrete syntax? 
Could someone else have a different opinion on this matter? 
Which of your functions would you have to modify to correctly handle this case?

Uhmmm comparando con una base de datos relacional... cuando se realiza un select especificando columnas que no existen,
la base de datos lanza una excepcion para evitar una repuesta que contenta null... si el ejemplo quiere manipular datos
creo que deberia manejar la misma filosifia.

Yo creo que si otra manera de resolverlo es que devuelva un Null o None, o algo que indique no existe respuesta pero ese tipo
de implementaciones a mi parecer provoca efectos secundarios.. porque si se puede recibir un null se tiene que hacer doble check
por los parametros que se reciben

Tendria que modificar checkRow y ver como mostrar un mensaje de error en Haskell... 
-}
