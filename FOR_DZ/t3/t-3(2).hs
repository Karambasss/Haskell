--Кузнецов Михаил Пи19-4
--Вариант 19
-- Список степеней двойки. ( по выбору)
--Функция makePositive, которая меняет знак всех отрицательных элементов списка чисел, например: 
--makePositive [-1, 0, 5, -10, -20] дает [1,0,5,10,20]

makePositive :: Num a => [a] -> [a] 
makePositive = map abs