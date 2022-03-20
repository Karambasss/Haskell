--Кузнецов Михаил Пи19-4
--Вариант 19
--Определите функцию, принимающую на вход целое число n и возвращающую список, содержащий n элементов.
--(одну задачу - по номеру, вторую - по выбору, G-документ t-2 )
-- Список биномиальных коэффициентов

binom :: Double -> [Double]
binom n | n < 1 = error "Not natural numbers are not allowed"
        | otherwise = binom_el (n-1) (n-1)
                    where binom_el n 0 = [1]
                          binom_el n k = [fac (n) / (fac (k) * fac (n-k))] ++ (binom_el n (k - 1))
                                where fac 0 = 1
                                      fac n = n * fac (n - 1)
