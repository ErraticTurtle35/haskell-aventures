import System.IO

type Field = String
type Row = [Field]
type Table = [Row]

readTable :: String -> IO ()
readTable filename = do 
    inh <- openFile (filename ++ ".txt") ReadMode
    rows <-loopRows inh
    hClose inh

loopRows :: Handle -> IO Table 
loopRows inh =
    do ineof <- hIsEOF inh
       if ineof
          then return []
          else do inpStr <- hGetLine inh
                  let row = words inpStr
                  putStrLn (show row)   
                  loopRows inh
                  
{-readTable :: String -> IO ()
readTable filename = do 
    inh <- openFile (filename ++ ".txt") ReadMode
    loopRows inh
    hClose inh

loopRows :: Handle -> IO Table 
loopRows inh =
    do ineof <- hIsEOF inh
       if ineof
          then return []
          else do inpStr <- hGetLine inh
                  let row = words inpStr
                  putStrLn (show row)   
                  loopRows inh


readTable' :: String -> IO ()
readTable' filename = do 
    inh <- openFile (filename ++ ".txt") ReadMode
    loopRows' inh
    hClose inh

loopRows' :: Handle -> IO Table 
loopRows' inh =
    do ineof <- hIsEOF inh
       if ineof
          then return []
          else return []-}

readFile' :: String -> IO String
readFile' fn = readFile (fn ++ ".txt") 


--["customer_id", "first_name", "last_name"]
--["order_id", "order_date", "amount", "customer_id"]
--[["first","last","gender","salary"],["Alice","Allen","female","82000"],["Bob","Baker","male","70000"],["Carol","Clarke","female","50000"],["Dan","Davies","male","45000"],["Eve","Evans","female","275000"]]
--maybe False (&& True) (Just (("gender" `elem` ["first","last","gender","salary"])))
--maybe [] (++ ["hola"]) (Just (("gender" `elem` ["first","last","gender","salary"])))
--[[["last","Allen","Baker","Clarke","Davies","Evans"]],[["first","Alice","Bob","Carol","Dan","Eve"]],[["salary","82000","70000","50000","45000","275000"]]]
