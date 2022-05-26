import Data.Map


data Arc a b = Arc {from :: a, term :: b, to :: a}


isStateGraphDeterministic :: (Ord a, Ord b) => [Arc a b] -> Bool
isStateGraphDeterministic graph = let
  check _ [] = True
  check arcSet (x:xs) = let
    k = (from x, term x)
    check' = not (member k arcSet && arcSet ! k /= to x) && check (insert k (to x) arcSet) xs
    in
      check'
  in
    check empty graph


next :: (Eq a, Eq b) => a -> b -> [Arc a b] -> a
next node t graph = let
  find [] = error "Bad graph."
  find (x:xs) = if from x == node && term x == t then to x else find xs
  in
    find graph



kau :: (Eq p, Eq a) => [Arc p a] -> p -> p -> p -> a -> [a] -> Bool
kau arcList begin trueEnd falseEnd emptySym s = let
    kau' fr (x:xs) = let
      t = next fr x arcList
      in
        if trueEnd == t then True
        else if falseEnd == t then False
        else kau' t xs
    kau' fr [] = let
      t = next fr emptySym arcList
      in
        if trueEnd == t then True
        else if falseEnd == t then False
        else error "Bad graph."
  in
    kau' begin s


st19 = [
  Arc "BEGIN" 'E' "FALSE",
  Arc "BEGIN" '0' "A",
  Arc "BEGIN" '1' "FALSE",
  Arc "A" 'E' "FALSE",
  Arc "A" '0' "B",
  Arc "A" '1' "FALSE",
  Arc "B" 'E' "FALSE",
  Arc "B" '0' "B",
  Arc "B" '1' "C",
  Arc "C" 'E' "FALSE",
  Arc "C" '0' "B",
  Arc "C" '1' "D",
  Arc "D" 'E' "FALSE",
  Arc "D" '0' "B",
  Arc "D" '1' "TRUE"
  ]

res19 = kau st19 "BEGIN" "TRUE" "FALSE" 'E'

-- 2 часть задания - проверка на то что ка детерминированный
st20 = [
  Arc "BEGIN" 'E' "FALSE",
  Arc "BEGIN" '0' "A",
  Arc "BEGIN" '1' "FALSE",
  Arc "A" 'E' "FALSE",
  Arc "A" '0' "B",
  Arc "A" '1' "FALSE",
  Arc "B" 'E' "FALSE",
  Arc "B" '0' "B",
  Arc "B" '1' "C",
  Arc "C" 'E' "FALSE",
  Arc "C" '0' "B",
  Arc "C" '0' "D",
  Arc "D" 'E' "FALSE",
  Arc "D" '0' "B",
  Arc "D" '1' "TRUE"
  ]
res20 = isStateGraphDeterministic st20

main :: IO()
main = do
    let string1 = "00101000110101101101111100"
    let string2 = "11001000110101101101111100"
    
    let string3 = "1010011100"
    let string4 = "0010011100"
    
    let string5 = "1110011111"
    let string6 = "0001100100000011"
    
    print $ res19 string1 -- выведет True
    print $ res19 string2 -- выведет False
    print $ res19 string3 -- выведет False
    print $ res19 string4 -- выведет True
    print $ res19 string5 -- выведет False
    print $ res19 string6 -- выведет False
    
    print $ "VTORAYA CHAST ZADANIA - PROVERKA NA TO, CHTO KA DETERMINIROVANNIY."
    print $ res20 -- выведет False, так как КА не детерминированный