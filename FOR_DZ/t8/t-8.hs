--Кузнецов Михаил Пи19-4
--Вариант 12
--Определите следующие функции с использованием функций высшего порядка:  Найти сумму двух векторов.

sumVectors :: Num a => [a] -> [a] -> [a]
sumVectors = zipWith (+) 

main :: IO()
main = do
    let list1 = [1,9,8,10]
    let list2 = [10,11,12,15]
    
    let list3 = [1.5,2.5,3.8,7.3]
    let list4 = [10,11,12,15]
    
    let list5 = [1.4,9,4.5,10]
    let list6 = [14.7,11,11.9,18.2]
    
    print $ sumVectors list1 list2
    print $ sumVectors list3 list4
    print $ sumVectors list5 list6
