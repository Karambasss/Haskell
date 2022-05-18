--Кузнецов Михаил Пи19-4
--Определите функцию eval, которая принимает два параметра: выражение типа Expr и список пар типа (String,Integer), задающий соответствие имен переменных и их значений. Функция должна вычислять значение выражение с учетом заданных значений выражений. Например, выражение eval (Add (Var "x") (Var "y")) [("x",1),("y",2)] должно выдавать число 3.
module T7Kuz where

data Expr = Add Expr Expr | Mult Expr Expr | Const Integer | Var String deriving (Eq,Show,Read)
 
eval :: Expr -> [(String,Integer)] -> Integer
eval (Const x) vlist = x
eval (Add e1 e2) vlist = (eval e1 vlist) + (eval e2 vlist)
eval (Mult e1 e2) vlist = (eval e1 vlist) * (eval e2 vlist)
eval (Var v) vlist = getVal vlist v
                     where getVal [] _ = error "Variable not found"
                           getVal (vl:vls) v | (v == fst vl) = snd vl
                                             | otherwise = getVal vls v

readExpr :: String -> IO Expr
readExpr inputData = if inputData == "1" then do
                                        putStrLn "Enter the expression "
                                        inputExpr <- getLine
                                        return (read inputExpr :: Expr)
                                      else do
                                        putStrLn "Enter the path to the file, it should be described by the expression: "
                                        inputPath <- getLine
                                        inputExpr <- readFile inputPath
                                        return (read inputExpr :: Expr) 
                                                    

defaultPathFileOut :: [Char]
defaultPathFileOut = "C:/Users/mpapa/Haskell/FOR_DZ/data/out.txt"

outResult :: [Char] -> String -> IO ()
outResult outType result = if outType == "1" then do
                             putStrLn result
                           else do
                             putStrLn "Specify the path to the file to write the response to: "
                             inputPathFileOut <- getLine
                             let pathFileOut = if inputPathFileOut == "" then defaultPathFileOut else inputPathFileOut  
                             writeFile pathFileOut result

                             putStrLn $ "The answer is written to a file (" ++ pathFileOut ++ ")"


readParams :: Read b => [Char] -> IO b
readParams inputType = if inputType == "1" then do
                                              putStrLn "Enter parameters for the expression:"
                                              inputParams <- getLine
                                              return (read inputParams)
                                            else do
                                              putStrLn "Enter the path to the file, it should contain the parameters: "
                                              inputPath <- getLine
                                              inputParams <- readFile inputPath
                                              return (read inputParams)



main :: IO()
main = do  
  putStrLn "How do you want to enter the expression?\n1 - via the console\n2 - via file"
  inputData <- getLine
  expr <- readExpr inputData
  
  putStrLn "How do you want to enter the shape?\n1 - via the console\n2 - via file"
  inputType <- getLine
  params <- readParams inputType
  
  let result = eval expr params
  putStrLn "How do you want to output the data?\n1 - to the console\n2 - to a file"
  inputOutType <- getLine
  outResult inputOutType $ show result

