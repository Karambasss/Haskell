--Кузнецов Михаил Пи19-4
--Вариант 19

--Задание 3. Определите следующую функцию
--9) Объединить 2 диапазона. Пример: f [2,5] [3,7] -> [2,7]

--Задание 3
union :: [Int] -> [Int] -> [Int]
union x y | ((head y) > (head $ tail x)) = error "These intervals don't cross"
          | ((head x) > (head $ tail y)) = error "These intervals don't cross"
          | otherwise = [min (head x) (head y), max (head $ tail x) (head $ tail y)]