--В электронной записной книжке хранятся записи следующих видов: напоминания о днях рождения знакомых, телефоны знакомых и назначенные встречи. Напоминание состоит из имени знакомого и даты (день и месяц). Запись о телефоне должна содержать имя человека и его телефон. Информация о назначенной встрече содержит дату встречи (день, месяц, год) и краткое описание (можно представлять строкой). Разработайте тип данных, представляющий такую запись. Записная книжка является списком записей.
--Определите следующие функции:
--1) getByName, возвращающая информацию о человеке с указанным именем (его телефон и дату рождения).
--2) getByLetter, возвращающая список людей, о которых есть информация в записной книжке и чье имя начинается на указанную букву.
--3) getAssignment, возвращающая по указанной дате список дел (информация о назначенных встречах и телефоны друзей, которых нужно поздравить в этот день).



data NoteBook = DenR String Int Int | Tel String Int | Vstrecha Int Int Int String deriving (Eq,Show)

getTel :: [NoteBook] -> String -> Int
getTel [] _ = 0
getTel ((Tel a b):xs) n = if a==n then b else getTel xs n
getTel (x:xs) n = getTel xs n

getDate :: [NoteBook] -> String -> [Int]
getDate [] _ = [0,0]
getDate ((DenR a b c):xs) n = if a==n then [b,c] else getDate xs n
getDate (x:xs) n = getDate xs n

getByName :: [NoteBook] -> String -> [Int]
getByName x name = if (getTel x name)==0 && (getDate x name)==[0,0] then error "net dostat info o cheloveke"
else (getTel x name) : (getDate x name)

getByLetter :: [NoteBook] -> Char -> [String]
getByLetter [] _ = []
getByLetter (x:xs) n = case x of
 DenR y _ _ -> if n==head(y) then y:getByLetter xs n else getByLetter xs n
 Tel y _ -> if n==head(y) then y:getByLetter xs n else getByLetter xs n
 Vstrecha _ _ _ _ -> getByLetter xs n

getVstr :: [NoteBook] -> Int -> Int -> Int -> [(String,[Int])]
getVstr [] _ _ _ = []
getVstr ((Vstrecha a1 b1 c1 z):xs) a b c = if (a==a1)&&(b==b1)&&(c==c1) then (z,[a,b,c]):getVstr xs a b c
else getVstr xs a b c
getVstr (x:xs) a b c = getVstr xs a b c


getDenR :: [NoteBook] -> Int -> Int -> [String]
getDenR [] _ _ = []
getDenR ((DenR c a1 b1):xs) a b = if (a==a1)&&(b==b1) then c:getDenR xs a b else getDenR xs a b
getDenR (x:xs) a b = getDenR xs a b

getTel2 :: [NoteBook] -> [String] -> [(String,[Int])]
getTel2 a [] = []
getTel2 a (x:xs) = if (getTel a x)==0 then getTel2 a xs
else (x,[(getTel a x)]):getTel2 a xs

getAssignment :: [NoteBook] -> Int -> Int -> Int -> [(String,[Int])]
getAssignment x a b c = (getVstr x a b c)++(getTel2 x (getDenR x a b))
