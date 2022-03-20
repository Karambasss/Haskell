--Определите функцию eval, которая принимает два параметра: выражение типа Expr и список пар типа (String,Integer), задающий соответствие имен переменных и их значений. Функция должна вычислять значение выражение с учетом заданных значений выражений. Например, выражение eval (Add (Var "x") (Var "y")) [("x",1),("y",2)] должно выдавать число 3.


data Expr = Add Expr Expr | Mult Expr Expr | Const Integer | Var String deriving (Eq,Show)
 
eval :: Expr -> [(String,Integer)] -> Integer
eval (Const x) vlist = x
eval (Add e1 e2) vlist = (eval e1 vlist) + (eval e2 vlist)
eval (Mult e1 e2) vlist = (eval e1 vlist) * (eval e2 vlist)
eval (Var v) vlist = getVal vlist v
                     where getVal [] _ = error "Variable not found"
                           getVal (vl:vls) v | (v == fst vl) = snd vl
                                             | otherwise = getVal vls v