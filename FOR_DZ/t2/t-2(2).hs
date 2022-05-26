--Кузнецов Михаил Пи19-4
--Вариант 19
-- Список степеней двойки. ( по выбору)

task2 :: Integer -> [Integer]
task2 0 = []
task2 n = if (n<0) then (error "bad number") 
else task2(n-1)++[2^n]

