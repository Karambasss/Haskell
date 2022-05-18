module T12Kuz where

--Кузнецов Михаил 
--Вариант 19
--Построить КА, допускающий цепочки из {0,1}*, в цепочке есть подцепочка 111 и она начинается на 00


isZeroOrOne :: Char -> Bool
isZeroOrOne x = x == '0' || x == '1'


zeroOneChain19 :: String -> Bool
zeroOneChain19 s = let
  match ('0':'0':xs) = find111 xs "111"
  match _ = False
  
  find111 _ [] = True
  find111 (x:xs) (p:ps)
    | x == p = find111 xs ps
    | otherwise = find111 xs "111" 
  find111 _ _ = False
  in
     match s && all isZeroOrOne s
     
main :: IO()
main = do
    let string1 = "00101000110101101101111100"
    let string2 = "11001000110101101101111100"
    
    let string3 = "1010011100"
    let string4 = "0010011100"
    
    let string5 = "1110011111"
    let string6 = "0001100100000011"
    
    print $ zeroOneChain19 string1 -- выведет True
    print $ zeroOneChain19 string2 -- выведет False
    print $ zeroOneChain19 string3 -- выведет False
    print $ zeroOneChain19 string4 -- выведет True
    print $ zeroOneChain19 string5 -- выведет False
    print $ zeroOneChain19 string6 -- выведет False