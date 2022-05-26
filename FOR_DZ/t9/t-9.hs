--Кузнецов Михаил Пи19-4
--Вариант 20
--Найти номер позиции максимального элемента в списке

import Data.List
import Data.Ord

maxi :: Ord a => [a] -> (a, Int)
maxi xs = maximumBy (comparing fst) (zip xs [0..])


main :: IO()
main = do
    let list1 = [1,9,8,10]
    let list2 = [10,11,12,15]
    
    let list3 = [1.5,2.5,3.8,7.3]
    let list4 = [10,11,12,15]
    
    let list5 = [1.4,9,4.5,10]
    let list6 = [14.7,11,11.9,18.2]
    
    print $ maxi list1  --(10,3)
    print $ maxi list2 -- (15,3)
    print $ maxi list3  --(7.3,3)
    print $ maxi list4 -- (15,3)
    print $ maxi list5  --(10.0,3)
    print $ maxi list6 -- (18.2,3)
