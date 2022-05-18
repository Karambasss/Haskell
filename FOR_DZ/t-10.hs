--Кузнецов Михаил Пи19-4
--Вариант 20
--Выделить из списка элементы на четных позициях


getEven :: [a] -> [a]
getEven (y:_:s) = y: getEven s
getEven (x:[]) = [x]
getEven [] = []

main :: IO()
main = do
    let list1 = [1,112,51,59,8,10,9]
    let list2 = [10,11,12,15]
    
    let list3 = [1.5,2.5,3.8,7.3]
    let list4 = [10,11,12,15]
    
    let list5 = [1.4,9,4.5,10]
    let list6 = [14.7,11,11.9,18.2]
    
    print $ getEven list1  --[1,51,8,9]
    print $ getEven list2 -- [10,12]
    print $ getEven list3  -- [1.5,3.8]
    print $ getEven list4 -- [10,12]
    print $ getEven list5  -- [1.4,4.5]
    print $ getEven list6 -- [14.7,11.9]
