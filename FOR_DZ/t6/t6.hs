--Кузнецов Михаил, Шихсаидов Шихсаид, Петрова Екатерина
--Задача 2. Написать программу решения систем линейных уравнений (СЛУ) методом Гаусса. См. Лекции_Haskell/EX2/matrix.hs - решение СЛУ методом Крамера. Сравнить результаты на тестах.


type Row = [Float]
type Matrix = [Row]
gaussianReduce :: Matrix -> Matrix
gaussianReduce matrix = fixlastrow $ foldl reduceRow matrix [0..length matrix-1] where
 
 --заменяет элемент в позиции a элементом в позиции b.
 swap xs a b
  | a > b = swap xs b a
  | a == b = xs
  | a < b = let
  (p1,p2) = splitAt a xs
  (p3,p4) = splitAt (b-a-1) (tail p2)
  in p1 ++ [xs!!b] ++ p3 ++ [xs!!a] ++ (tail p4)
 
 reduceRow matrix1 r = let
  --первый ненулевой элемент на или ниже (r,r).
  firstnonzero = head $ filter (\x -> matrix1 !! x !! r /= 0) [r..length matrix1-1]
 
  --матрица с заменой строк (при необходимости)
  matrix2 = swap matrix1 r firstnonzero
 
  --ряд, с которым мы работаем
  row = matrix2 !! r
 
  --делает так, чтобы был 1 ведущий коэффициент
  row1 = map (\x -> x / (row !! r)) row
 
  --вычитает nr из строки 1 при умножении
  subrow nr = let k = nr!!r in zipWith (\a b -> k*a - b) row1 nr
 
  --применяет подстроку ко всем строкам ниже
  nextrows = map subrow $ drop (r+1) matrix2
 
  --объединяет списки и повторяет
  in take r matrix2 ++ [row1] ++ nextrows
 
 fixlastrow matrix' = let
  a = init matrix'; row = last matrix'; z = last row; nz = last (init row)
  in a ++ [init (init row) ++ [1, z / nz]]

substitute :: Matrix -> Row
substitute matrix = foldr next [last (last matrix)] (init matrix) where
 
 next row found = let
  subpart = init $ drop (length matrix - length found) row
  solution = last row - sum (zipWith (*) found subpart)
  in solution : found

solve :: Matrix -> Row
solve = substitute . gaussianReduce

main :: IO()
main = do
    let list1 = [[1,-5,-1,2], [0,4,3,0], [0,0,19,20]]
    let list2 = [[2, 10, -3, 38], [-3, -24, 5, -86], [1, 3, -5, 27]]
    let list3 = [[1, 0, 1, 0], [-1, 1, -2, 1], [-4, 4, 0, 1]]

    print $ solve list1
    print $ solve list2
    print $ solve list3

