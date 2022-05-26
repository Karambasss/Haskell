--Кузнецов Михаил Пи19-4
--Область на плоскости является либо прямоугольником, либо кругом, либо объединением областей, либо их пересечением. Прямоугольник характеризуется координатами левого нижнего и правого верхнего углов, круг — координатами центра и радиусом. Разработайте структуру данных, представляющую область описанного вида. Определите следующие функции:
--contains, проверяющая, что заданная точка попадает в область.
module T7Kuz2 where

type Point = (Int,Int)
 
data Figura =  Empty
             | Rect Point Point
             | Circle Point Int
             | Union Figura Figura
             | Intersection Figura Figura
        deriving (Show, Read)
 
contains:: Point -> Figura -> Bool
contains (x0,y0) = go 
   where go Empty = False 
         go (Rect (l,b) (r,t)) = (l<=x0) && (r>=x0) && (b<=y0) && (t>=y0) 
         go (Circle (x,y) r) =((x-x0)^2 + (y-y0)^2)<=r^2
         go (Union f1 f2) = (go f1) || (go f2)
         go (Intersection f1 f2) = (go f1) && (go f2)
testdata = Union (Rect (10,20) (30,50)) (Intersection (Circle (20,25) 5) (Rect (15,15) (20,25)))



readPoint :: String -> IO Point
readPoint inputData = if inputData == "1" then do
                                        putStrLn "Enter a point (x, y):"
                                        input <- getLine
                                        return (read input)
                                      else do
                                        putStrLn "Enter the path to the file, it should contain a dot (x, y): "
                                        inputPath <- getLine
                                        input <- readFile inputPath
                                        return (read input) 
                                        
                                        
readFigure :: String -> IO Figura
readFigure inputData = if inputData == "1" then do
                                        putStrLn "Enter a description of the shape :"
                                        input <- getLine
                                        return (read input)
                                      else do
                                        putStrLn "Enter the path to the file, it should describe the shape:"
                                        inputPath <- getLine
                                        input <- readFile inputPath
                                        return (read input) 
                                        

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


main :: IO()
main = do
  
    putStrLn "How do you want to enter a point?\n1 - via the console\n2 - via file"
    inputData <- getLine
    point <- readPoint inputData
    
    putStrLn "How do you want to enter the shape?\n1 - via the console\n2 - via file"
    inputType <- getLine
    figure <- readFigure inputType
    
    let result = contains point figure
    putStrLn "How do you want to output the data?\n1 - to the console\n2 - to a file"
    inputOutType <- getLine
    outResult inputOutType $ show result
      
--    print $ contains (17,20) testdata
--    print $ contains (15,20) testdata
--    print $ contains (15,19) testdata
--    print $ contains (10,20) testdata
--    print $ contains (14,26) testdata
--    print $ contains (9,20)  testdata
    

