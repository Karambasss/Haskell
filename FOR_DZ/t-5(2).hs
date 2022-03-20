--Область на плоскости является либо прямоугольником, либо кругом, либо объединением областей, либо их пересечением. Прямоугольник характеризуется координатами левого нижнего и правого верхнего углов, круг — координатами центра и радиусом. Разработайте структуру данных, представляющую область описанного вида. Определите следующие функции:
--contains, проверяющая, что заданная точка попадает в область.



type Point = (Int,Int)
 
data Figura =  Empty
             | Rect Point Point
             | Circle Point Int
             | Union Figura Figura
             | Intersection Figura Figura
        deriving Show
 
contains:: Point -> Figura -> Bool
contains (x0,y0) = go 
   where go Empty = False 
         go (Rect (l,b) (r,t)) = (l<=x0) && (r>=x0) && (b<=y0) && (t>=y0) 
         go (Circle (x,y) r) =((x-x0)^2 + (y-y0)^2)<=r^2
         go (Union f1 f2) = (go f1) || (go f2)
         go (Intersection f1 f2) = (go f1) && (go f2)
testdata = Union (Rect (10,20) (30,50)) (Intersection (Circle (20,25) 5) (Rect (15,15) (20,25)))