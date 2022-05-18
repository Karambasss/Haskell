module T11Kuz where
--Кузнецов Михаил 
--Вариант 3
--Удалить из программы комментарии. Для исходного файла проверяется расширение (если расширение не соответствует, ничего не делать) на С++ (расширение *.сpp), комментарии // …..

splitOn :: String -> String -> (String, String)   
splitOn sub str = let
  splitOnA' [] s = ([], s)
  splitOnA' (y:ys) (x:xs)
    | x == y = let (xs', xs'') = splitOnA' ys xs in (xs', xs'')
    | otherwise = let (xs', xs'') = splitOnA' sub xs in (x:xs', xs'')
  splitOnA' _ _ = ([], [])
    in splitOnA' sub str


splitFilePath :: String -> (String, String)
splitFilePath = splitOn "."

cleanComments :: String -> String
cleanComments = let

  walkState [] = []
  walkState ('\n':xs) = check1State xs
  walkState ('/':xs) =  check2State xs
  walkState (x:xs) = x : walkState xs
  
  check1State ('/':xs) = check2State xs
  check1State s = '\n': walkState s
  
  check2State ('/':xs) = cleanState xs
  check2State s = walkState s
  
  cleanState = walkState . dropWhile (/= '\n')
  
  in
    walkState 


main :: IO ()
main = do
  putStrLn "Enter the path to the file for which you want to delete comments: "
  fileInPath <- getLine
  let (fName, fExt) = splitFilePath fileInPath
  let fileOutPath = fName ++ "_clean" ++ "." ++ fExt

  if fExt /= "cpp" 
    then error "File is not .cpp" 
  else do
    content <- readFile fileInPath
    writeFile fileOutPath (cleanComments content)
