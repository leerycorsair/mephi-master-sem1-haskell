module HW4 where

import Data.ByteString.Char8 qualified as C
import Data.List (sort)
import GHC.Base (VecElem (Int16ElemRep))
import GHC.Exts.Heap (GenClosure (value))

-------------------------------------------------------------------------------
-- Упр. 1: Реализовать функцию pascal c r  для вычисления значений из
-- треугольника паскаля. Здесь c – это номер столбца, а r – номер строки.
-- Столбцы и строки отсчитываются начиная с нуля.
--
-- Примеры:
--   pascal 0 2 = 1
--   pascal 1 2 = 2
--   pascal 2 4 = 6

-- Ответ:
pascal :: Int -> Int -> Int
pascal c r = (pascalRow r) !! (c - 1)
  where
    pascalRow :: Int -> [Int]
    pascalRow 1 = [1]
    pascalRow row = zipWith (+) ([0] ++ pascalRow (row - 1)) (pascalRow (row - 1) ++ [0])

-- возвращает строковое представление первых n строчек треугольника паскаля.
printIt :: Int -> C.ByteString
printIt n = C.pack $ show $ [pascal y x | x <- [0 .. n], y <- [0 .. x]]

-- печатает первые n строчек треугольника паскаля в консоль.
printItIo :: Int -> IO ()
printItIo n = mapM_ print [[pascal y x | y <- [0 .. x]] | x <- [0 .. n]]

-------------------------------------------------------------------------------
-- Упр. 3: Реализуйте функцию, которая находит наибольший общий делитель своих
-- аргументов.

-- Ответ:
gcd' :: Int -> Int -> Int
gcd' a 0 = abs a
gcd' a b = gcd' b (mod a b)

-------------------------------------------------------------------------------
-- Упр. 4: Реализуйте функцию delete, которая принимает на вход строку и
-- символ и возвращает строку, в которой удалены все вхождения символа.

-- Пример:
--   delete ’l’ "Hello world!"  ==> "Heo word!"

-- Ответ:
delete :: Char -> String -> String
delete ch str = filter (/= ch) str

-------------------------------------------------------------------------------
-- Упр. 5: Реализуйте функцию substitute, которая заменяет в строке указанный
-- символ на заданный.

-- Пример:
--   substitute ’e’ ’i’ "eigenvalue" ==> "iiginvalui"

-- Ответ:
substitute :: Char -> Char -> String -> String
substitute ch1 ch2 str = map (\chr -> if chr == ch1 then ch2 else chr) str

-------------------------------------------------------------------------------
-- Упр. 6: Реализуйте с использованием хвостовой рекурсии функцию reverse',
-- которая обращает список.

-- Замечание : Все необходимые вспомогательные функции должны быть определены
-- при помощи let-выражения.

-- Пример:
-- reverse' [] ==> []
-- reverse' [1,6,4,2] => [2,4,6,1]

-- Ответ:
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (head : tail) = reverse' tail ++ [head]

-------------------------------------------------------------------------------
-- Упр. 7: Не используя какие-либо встроенные функции, реализуйте с
-- использование хвостовой рекурсии функцию assoc.
-- Такую, что
--   assoc def key [(k1,v1), (k2,v2), (k3,v3);...])
-- ищет первое i, такое что ki = key. Если такое ki было найдено, то значение
-- функции – vi, иначе значение функции равно def.

-- Примеры:
--   assoc 0 "william" [("ranjit", 85), ("william",23), ("moose",44)] ==> 23
--   assoc 0 "bob" [("ranjit",85), ("william",23), ("moose",44)]      ==> 0

-- Ответ:
assoc :: Int -> String -> [(String, Int)] -> Int
assoc def key [] = def
assoc def key (head : tail)
  | fst head == key = snd head
  | otherwise = assoc def key tail

-------------------------------------------------------------------------------
-- Упр. 8: Напишите рекурсивную функцию balance, проверяющую
-- балансировку скобок в строке.
-- Как вы должны помнить, строки в Haskell — это список символов [Char].
-- Например, для следующих строк функция должна вернуть True:
--   (if (zero? x) max (/ 1 x))
--   I told him (that it’s not (yet) done). (But he wasn’t listening)
-- А для строк ниже, напротив, функция должна вернуть значение False:
--   :-)
--   ())(
-- Последний пример демонстрирует: недостаточно проверить только, что строка содержит
-- равное количество открывающих и закрывающих скобок.

-- Ответ:
balance :: String -> Bool
balance str = checkParentheses str []
  where
    checkParentheses :: String -> [Char] -> Bool
    checkParentheses [] stack = null stack
    checkParentheses (c : cs) stack
      | isOpening c = checkParentheses cs (c : stack)
      | isClosing c = case stack of
          [] -> False
          (x : xs) -> isOpeningPair x c && checkParentheses cs xs
      | otherwise = checkParentheses cs stack
    isOpening :: Char -> Bool
    isOpening c = c `elem` "({["
    isClosing :: Char -> Bool
    isClosing c = c `elem` ")}]"
    isOpeningPair :: Char -> Char -> Bool
    isOpeningPair x y = (x == '(' && y == ')') || (x == '{' && y == '}') || (x == '[' && y == ']')

-------------------------------------------------------------------------------
-- Упр. 9.a:
-- Треугольное число – число монет, которые можно расставить в виде правильного
-- треугольника со стороной n.
--
-- Пример:
--   n1 = 1:      o
--
--   n2 = 3:      o
--               o o
--
--   n3 = 6:      o
--              o  o
--             o  o  o
-- и т.д.
--
-- Напишите функцию, которая строит список треугольных чисел.

-- Ответ:
triangulars :: Int -> [Int]
triangulars n = map triangular [1 .. n]
  where
    triangular :: Int -> Int
    triangular 1 = 1
    triangular n = n + triangular (n - 1)

-------------------------------------------------------------------------------
-- Упр. 9.b: Пирамидальное число (тэтраэдальное) –  количество шариков, которые
-- можно расположить в виде пирамиды с треугольной равносторонней гранью, на
-- ребре которой распложено n шариков.
--
-- Напишите функцию, которая строит список пирамидальных чисел.

-- Ответ:
pyramidals :: Int -> [Int]
pyramidals n = map pyramidal [1 .. n]
  where
    pyramidal :: Int -> Int
    pyramidal 1 = 1
    pyramidal n = sum (triangulars n)
    triangulars :: Int -> [Int]
    triangulars n = map triangular [1 .. n]
    triangular :: Int -> Int
    triangular 1 = 1
    triangular n = n + triangular (n - 1)

-------------------------------------------------------------------------------
-- Упр. 10: Напишите рекурсивную функцию, которая подсчитывает число способов
-- разменять сумму с использованием заданного списка номиналов монет.
-- Например, есть 3 способа разменять 4, если у вас есть монеты достоинством
-- 1 и 2: 1+1+1+1, 1+1+2, 2+2.
-- Для выполнения задания реализуйте функцию, которая принимает сумму для
-- размена и список уникальных номиналов монет, а возвращает число способов
-- разменять данную сумму с использованием данных номиналов

-- Ответ:
countChange :: Int -> [Int] -> Int
countChange value coinsList = changeVariations value (reverse (sort (coinsList)))
  where
    changeVariations 0 _ = 1
    changeVariations _ [] = 0
    changeVariations value (highestCoin : otherCoins)
      | highestCoin > value = changeVariations value otherCoins
      | otherwise = changeVariations (value - highestCoin) (highestCoin : otherCoins) + changeVariations value otherCoins

-------------------------------------------------------------------------------
-- Упр. 11: Используя функции ФВП над списками: map, filter, а также
-- вспомогательные функции, такие как sum, product, even и т.д. напишите
-- выражение, которое:
--     a.  Вычисляет сумму квадратов элементов списка [1..10]
--     b.  Вычисляет произведение чётных чисел в списке [4,5,-2,10,11,4,5,8,6]

-- Ответ:
answer11a :: Int
answer11a = sum (map (\x -> x * x) [1 .. 10])

answer11b :: Int
answer11b = product (filter even [4, 5, -2, 10, 11, 4, 5, 8, 6])

-------------------------------------------------------------------------------
-- Упр. 12: Перепишите выражение используя сечения:
--     a.  map (\x -> x + 5) [1..10]
--     b.  filter (\y -> 5 > y) [3..7]

-- Ответ:
answer12a :: [Int] -> [Int]
answer12a = map (+ 5)

answer12b :: [Int] -> [Int]
answer12b = filter (< 5)

-------------------------------------------------------------------------------
-- Упр. 13: Если определить тип:
--               data Potato = Tomato Int Potato (String -> Int),
-- какой тип будет у Tomato?
--      a.  Potato
--      b.  Int -> Potato -> String -> Int -> Potato
--      c.  Int -> Potato -> (String -> Int) -> Potato
--      d.  Tomato
--      e.  Int -> Potato -> (String -> Int) -> Tomato
--      f.  Int -> Potato -> String -> Int -> Tomato

-- Ответ:
answer13a :: Char
answer13a = 'e'

-- answer13b :: Char
-- answer13b = undefined

-- answer13c :: Char
-- answer13c = undefined

-- answer13d :: Char
-- answer13d = undefined

-- answer13e :: Char
-- answer13e = undefined

-- answer13f :: Char
-- answer13f = undefined

-------------------------------------------------------------------------------
-- Упр. 14: Если определить тип:
--              data ThreeList = ThreeList [a] [b] [c],
-- какой тип будет у ThreeList?
--      a.  [a] -> [b] -> [c] -> ThreeList
--      b.  a -> b -> c -> ThreeList a b c
--      c.  [a] -> [b] -> [c] -> ThreeList [a] [b] [c]
--      d.  [a] -> [b] -> [c] -> Three List a b c

-- Ответ:
answer14a :: Char
answer14a = 'c'

-- answer14b :: Char
-- answer14b = undefined

-- answer14c :: Char
-- answer14c = undefined

-- answer14d :: Char
-- answer14d = undefined

-------------------------------------------------------------------------------
-- Упр. 15: Если определить тип:
--              data TwoList a b = TwoList {aList :: [a], b:List :: [b]}
-- какой тип будет у функции bList?
--      a.  bList – это не функция
--      b.  TwoList a b -> [b]
--      c.  [b] -> TwoList a b
--      d.  [b]

-- Ответ:
answer15a :: Char
answer15a = 'b'

-- answer15b :: Char
-- answer15b = undefined

-- answer15c :: Char
-- answer15c = undefined

-- answer15d :: Char
-- answer15d = undefined

-------------------------------------------------------------------------------
-- Упр. 16: Определите тип RainbowColor, значения которого – семь цветов
-- радуги. Параметров у конструкторов значений никаких нет.

-- Ответ:
data RainbowColor
  = Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Indigo
  | Violet

-------------------------------------------------------------------------------
-- Упр. 17: Определите тип BusTicket, который может представлять значения вида:
-- a.  SingleTicket
-- b.  MonthlyTicket "January"
-- c.  MonthlyTicket "December"

-- Ответ:
data BusTicket
  = SingleTicket
  | MonthlyTicket String

-------------------------------------------------------------------------------
-- Упр. 18: Определите тип данных Position, который состоит из двух
-- значений x и y типа Int. Также определите функции ниже для управления
-- значениями типа Position.
--
-- Примеры:
--   getY (up (up origin))    ==> 2
--   getX (up (right origin)) ==> 1

-- Ответ:
data Position = Position {x :: Int, y :: Int}
  deriving (Show)

-- origin – это Position с x и y равными 0
origin :: Position
origin = Position 0 0

-- getX возвращает значение x из Position
getX :: Position -> Int
getX = x

-- getY возвращает значение y из Position
getY :: Position -> Int
getY = y

-- up увеличивает значение y у Position на 1
up :: Position -> Position
up p = Position (getX p) (getY p + 1)

-- right увеличивает значение x у Position на 1
right :: Position -> Position
right p = Position (getX p + 1) (getY p)

-------------------------------------------------------------------------------
-- Упр. 19: Дано определение типа данных Nat – т.н. представление Пеано для
-- натуральных чисел. Суть представления в том, что есть константа Zero,
-- которая соответствует нулю и функция для получения следующего числа PlusOne.
-- Реализуйте функции fromNat и toNat, которые конвертируют значения типа Nat в Int
-- и обратно.
--
-- Примеры:
--   fromNat (PlusOne (PlusOne (PlusOne Zero)))  ==>  3
--   toNat 3    ==> Just (PlusOne (PlusOne (PlusOne Zero)))
--   toNat (-3) ==> Nothing

-- Ответ:
data Nat = Zero | PlusOne Nat
  deriving (Show, Eq)

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (PlusOne nutNumber) = 1 + fromNat nutNumber

toNat :: Int -> Maybe Nat
toNat 0 = Just Zero
toNat normalNumber
  | normalNumber > 0 = fmap PlusOne (toNat (normalNumber - 1))
  | otherwise = Nothing

-------------------------------------------------------------------------------
-- Упр. 20: Дано определение типа данных Bin – обращенная форма бинарного
-- представления целого числа.
--   Конструктор O   : представляет бит ноль, а его параметр – это остальная
--                     часть бинарного числа.
--   Конструктор I   : представляет бит один, его параметр – это также остальная
--                     часть бинарного числа.
--   Конструктор End : представляет конец бинарного числа. Он не принимает
--                     параметров.
-- Для упрощения вычислений биты бинарного числа записываются при помощи типа
-- данных Bin в обратном порядке:
--     1011 ==> I (I (O (I End)))
--     110  ==> O (I (I End))
--     10010110 ==> O (I (I (O (I (O (O (I End)))))))
-- Реализуйте следующие функции: prettyPrint, fromBin, toBin, которые
-- конвертируют Bin в человеко-читаемое строковое представление,
-- Bin в Int и Int в Bin соответственно.
--
-- Примеры:
--   prettyPrint End      ==> ""
--   prettyPrint (O End)  ==> "0"
--   prettyPrint (I End)  ==> "1"
--   prettyPrint (O (O (I (O (I End))))) ==> "10100"
--
--   map fromBin [
--     O End, I End,
--     O (I End),
--     I (I End),
--     O (O (I End)),
--     I (O (I End))
--   ] ==> [0, 1, 2, 3, 4, 5]
--       fromBin (I (I (O (O (I (O (I (O End)))))))) ==> 83
--       fromBin (I (I (O (O (I (O (I End)))))))     ==> 83
--
--   map toBin [0..5] ==> [
--     O End,
--     I End,
--     O (I End),
--     I (I End),
--     O (O (I End)),
--     I (O (I End))
--   ]
--   toBin 57 ==> I (O (O (I (I (I End)))))

-- Ответ:
data Bin = End | O Bin | I Bin deriving (Show, Eq)

-- функция, которая увеличивает бинарное число на один
inc :: Bin -> Bin
inc End = I End
inc (O b) = I b
inc (I b) = O (inc b)

prettyPrint :: Bin -> String
prettyPrint End = ""
prettyPrint (O rest) = prettyPrint rest ++ "0"
prettyPrint (I rest) = prettyPrint rest ++ "1"

fromBin :: Bin -> Int
fromBin End = 0
fromBin n = helper n 0 0
  where
    helper :: Bin -> Int -> Int -> Int
    helper End _ value = value
    helper (O rest) pow value = helper rest (pow + 1) value
    helper (I rest) pow value = helper rest (pow + 1) (value + 2 ^ pow)

toBin :: Int -> Bin
toBin 0 = End
toBin n
  | even n = O (toBin (div n 2))
  | otherwise = I (toBin (div n 2))