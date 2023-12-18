{-# LANGUAGE EmptyDataDecls #-}

module HW5 where

import Data.Functor
import Data.Set (powerSet)
import GHC.Base (Applicative (liftA2))
import Text.Read

data NeedDefineTypeWith a

data NeedDefineType

----------------------------------------------------------------------------------
-- Упр 1. Приведите примеры функций (с реализацией), который имеют следующие типы

-- Ответ:
f1 :: Float -> (Float -> Float)
f1 y = (* y)

f2 :: (Float -> Float) -> (Float -> Float)
f2 func = func . func

f3 :: a -> (a -> b) -> b
f3 value func = func value

f4 :: a -> b -> b
f4 x y = y

f5 :: (a -> a -> b) -> a -> b
f5 func value = func value value

f6 :: (a -> b) -> (b -> c) -> (a -> c)
f6 func1 func2 = func2 . func1

f7 :: (a -> b -> c) -> (a -> b) -> (a -> c)
f7 func1 func2 = \x -> func1 x (func2 x)

f8 :: (a -> b -> b) -> b -> [a] -> b
f8 func value [] = value
f8 func value (head : tail) = func head (f8 func value tail)

f9 :: (b -> a -> b) -> b -> [a] -> b
f9 func value [] = value
f9 func value (head : tail) = f9 func (func value head) tail

----------------------------------------------------------------------------------
-- Упр 2. Реализуйте функции:
--  a) curry'   - делает из некаррированной функции каррированную.
--  b) uncurry’ - делает из каррированной функции некаррированную.
--                Используя свёртки определите следующие функции

-- Ответ:
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' func x1 x2 = func (x1, x2)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' func params = func (fst params) (snd params)

----------------------------------------------------------------------------------
-- Упр 3. Используя свёртки определите функции:

-- Ответ:
concat' :: [[a]] -> [a]
concat' [] = []
concat' (row1 : others) = row1 ++ concat' others

inits :: [a] -> [[a]] -- список начал списка
inits [] = [[]]
inits (head : tail) = [] : map (head :) (inits tail)

tails :: [a] -> [[a]] -- список хвостов списка
tails [] = [[]]
tails list@(head : tail) = list : tails tail

----------------------------------------------------------------------------------
-- Упр 4. Определите алгебраический тип данных Set a, который определяет
-- множество элементов типа a.
--
-- Напоминание:
--    Множество – это совокупность объектов, хорошо различимых нашей интуицией и
--                мыслимых как единое целое.
--    Следствие: Любой элемент может входить в множество только один раз!
--
-- Определите функцию subset, которая проверяет, что все элементы первого
-- множества также являются элементами второго множества.
--
-- Используя функцию subset определите экземпляр класса Eq для типа Set a

-- Ответ:
data Set a = EmptySet | Set a (Set a) deriving (Show)

subset :: (Eq a) => Set a -> Set a -> Bool
subset EmptySet _ = True
subset (Set value set1Tail) set2 =
  isMember value set2 && subset set1Tail set2
  where
    isMember :: (Eq a) => a -> Set a -> Bool
    isMember _ EmptySet = False
    isMember value1 (Set value2 setTail) = value1 == value2 || isMember value1 setTail

-- достаточно реализовать только одну функцию (==) или (/=)
instance (Eq a) => Eq (Set a) where
  (==) :: Set a -> Set a -> Bool
  (==) set1 set2 = subset set1 set2 && subset set2 set1

----------------------------------------------------------------------------------
-- Упр 5. Определите класс типов Finite, который имеет только один метод:
--        получение списка всех элементов заданного типа. Идея в том, чтобы
--        такой список был конечным.
--
--        Определите экземпляры класса типов Finite для следующих типов:
--        • Bool
--        • (a, b) для конечных типов a и b
--        • Set a (из предыдущего упражнения), где a – конечный тип.
--        • a -> b, для всяких конечных типов a и b, где тип a также поддерживает
--          равенство. Используя полученное определение создайте также экземпляр
--          класса Eq для типа a -> b.

-- Ответ:
class Finite a where
  values :: [a]

instance Finite Bool where
  values :: [Bool]
  values = [False, True]

instance (Finite a, Finite b) => Finite (a, b) where
  values :: [(a, b)]
  values = [(x, y) | x <- values, y <- values]

instance (Finite a, Eq a) => Finite (Set a) where
  values :: [Set a]
  values = powerSet (values :: [a])
    where
      powerSet :: [a] -> [Set a]
      powerSet [] = [EmptySet]
      powerSet (head : tail) = powerSet tail ++ map (Set head) (powerSet tail)

instance (Finite a, Eq a, Finite b) => Finite (a -> b) where
  values :: [a -> b]
  values = [(\x -> value12 x) | value12 <- helper]
    where
      helper :: (Finite a, Eq a, Finite b) => [a -> b]
      helper = [const value2 | value2 <- values :: [b]]

----------------------------------------------------------------------------------
-- Упр 6. Определите алгебраический тип данных Complex для комплексных чисел.
--        Создайте селекторы realPart и imagPart, которые возвращают действительную
--        и мнимую части комплексного числа соответственно. Complex должен быть
--        экземпляром классов типов Eq и Show.
--
--        Определите экземпляр класс типов Num для типа Complex.

-- Ответ:
data Complex = Complex {rValue, iValue :: Double}

instance Eq Complex where
  (==) :: Complex -> Complex -> Bool
  (==) c1 c2 = rValue c1 == rValue c2 && iValue c1 == iValue c2

instance Show Complex where
  show :: Complex -> String
  show c = show (rValue c) ++ (if iValue c >= 0 then "+" ++ show (iValue c) else show (iValue c)) ++ "*i"

instance Num Complex where
  (+) :: Complex -> Complex -> Complex
  (+) c1 c2 = Complex (rValue c1 + rValue c2) (iValue c1 + iValue c2)
  (*) :: Complex -> Complex -> Complex
  (*) c1 c2 = Complex (rValue c1 * rValue c2 - iValue c1 * iValue c2) (rValue c1 * iValue c2 + iValue c1 * rValue c2)
  abs :: Complex -> Complex
  abs c = Complex (sqrt $ rValue c ** 2 + iValue c ** 2) 0
  signum :: Complex -> Complex
  signum (Complex 0 0) = Complex 0 0
  signum c = Complex (rValue c / rValue (abs c)) (iValue c / rValue (abs c))
  fromInteger :: Integer -> Complex
  fromInteger num = Complex (fromInteger num) 0
  negate :: Complex -> Complex
  negate c = Complex (-(rValue c)) (-(iValue c))

-------------------------------------------------------------------------------
-- Упр 7. Реализуйте функцию IncrementAll, которая получает контейнер-функтор,
-- содержащий числа и увеличивает каждое число в контейнере на 1.
--
-- Примеры:
--   incrementAll [1,2,3]     ==>  [2,3,4]
--   incrementAll (Just 3.0)  ==>  Just 4.0

-- Ответ:
incrementAll :: (Functor f, Num n) => f n -> f n
incrementAll = fmap (+ 1)

--------------------------------------------------------------------------------
-- Упр. 8. Реализуйте функции fmap2 и fmap3, которые выполняют отображение для
-- вложенных функторов.
--
-- Примеры:
--   fmap2 on [[Int]]:
--     fmap2 negate [[1,2],[3]]
--       ==> [[-1,-2],[-3]]
--   fmap2 on [Maybe String]:
--     fmap2 head [Just "abcd",Nothing,Just "efgh"]
--       ==> [Just 'a',Nothing,Just 'e']
--   fmap3 on [[[Int]]]:
--     fmap3 negate [[[1,2],[3]],[[4],[5,6]]]
--       ==> [[[-1,-2],[-3]],[[-4],[-5,-6]]]
--   fmap3 on Maybe [Maybe Bool]
--     fmap3 not (Just [Just False, Nothing])
--       ==> Just [Just True,Nothing]

-- Ответ:
fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 func = fmap (fmap func)

fmap3 :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
fmap3 func = fmap2 (fmap func)

------------------------------------------------------------------------------
-- Упр. 9. Напишите функцию, которая складывает два Maybe Int используя
-- операторы Applicative (liftA2 и pure). Не используйте сравнение с образцом
--
-- Примеры:
--  sumTwoMaybes (Just 1) (Just 2)  ==> Just 3
--  sumTwoMaybes (Just 1) Nothing   ==> Nothing
--  sumTwoMaybes Nothing Nothing    ==> Nothing

-- Ответ:
sumTwoMaybes :: Maybe Int -> Maybe Int -> Maybe Int
sumTwoMaybes = liftA2 (+)

------------------------------------------------------------------------------
-- Упр. 10. Напишите функцию, которая получает операцию (double или negate) и число
-- в виде строк. Функция должна вычислить результат операции.
-- Если передано неизвестное имя функции или некорректное число, то функция
-- должна вернуть Nothing
--
-- Подсказка: воспользуйтесь функцией readMaybe из модуля Text.Read
--
-- Примеры:
--  calculator "negate" "3"   ==> Just (-3)
--  calculator "double" "7"   ==> Just 14
--  calculator "doubl" "7"    ==> Nothing
--  calculator "double" "7x"  ==> Nothing

-- Ответ:
calculator :: String -> String -> Maybe Int
calculator op number
  | op == "negate" = fmap (* (-1)) (readMaybe number)
  | op == "double" = fmap (* 2) (readMaybe number)
  | otherwise = Nothing

-------------------------------------------------------------------------------
-- Упр. 11. Какое выражение эквивалентно do-блоку:
--            do y <- z
--               s y
--               return (f y)
-- Варианты ответа:
--          a.  z >> \y -> s y >> return (f y)
--          b.  z >>= \y -> s y >> return (f y)
--          c.  z >> \y -> s y >>= return (f y)

-- Ответ:
answer11 :: Char
answer11 = 'b'

------------------------------------------------------------------------------
-- Упр. 12. Какой тип у выражения: \x xs -> return (x : xs)./
-- Варианты ответа:
--          a.  Monad m => a -> [a] -> m [a]
--          b.  Monad m => a -> [m a] -> [m a]
--          c.  a -> [a] -> Monad [a]

-- Ответ:
answer12 :: Char
answer12 = 'a'

------------------------------------------------------------------------------
-- Упр. 13. Тип Result ниже работает почти как Maybe, но в отличие от
-- последнего имеет два типа для обозначения отсутствия значения: один без и
-- один с описанием.
--
-- Реализуйте экземпляры классов типов Functor, Monad и Applicative для Result.
--
-- Result ведёт себя как тип Maybe в том смысле, что
-- 1) MkResult - это как Just
-- 2) Если в процессе вычисления возникает NoResult, то всё вычисление
--    даёт в результате NoResult (как Nothing)
-- 3) Также, если было получено значение Failure "причина", то результат
--    всего вычисления будет Failure "reason"
--
-- Примеры:
--   MkResult 1 >> Failure "boom" >> MkResult 2
--     ==> Failure "boom"
--   MkResult 1 >> NoResult >> Failure "not reached"
--     ==> NoResult
--   MkResult 1 >>= (\x -> MkResult (x+1))
--     ==> MkResult 2

-- Ответ:
data Result a = MkResult a | NoResult | Failure String
  deriving (Show)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap f NoResult = NoResult
  fmap f (Failure str) = Failure str
  fmap f (MkResult a) = MkResult (f a)

instance Applicative Result where
  pure :: a -> Result a
  pure = MkResult
  (<*>) :: Result (a -> b) -> Result a -> Result b
  (<*>) _ NoResult = NoResult
  (<*>) NoResult _ = NoResult
  (<*>) _ (Failure str) = Failure str
  (<*>) (Failure str) _ = Failure str
  (<*>) (MkResult f) (MkResult x) = MkResult $ f x

instance Monad Result where
  return :: a -> Result a
  return = pure
  (>>=) :: Result a -> (a -> Result b) -> Result b
  (>>=) NoResult _ = NoResult
  (>>=) (Failure str) _ = Failure str
  (>>=) (MkResult x) f = f x

------------------------------------------------------------------------------
-- Упр. 14. Ниже дана реализациия списков а-ля Haskell. Реализуйте
-- экземпляры классов типов Functor, Applicative и Monad для List.
--
-- Пример:
--   fmap (+2) (LNode 0 (LNode 1 (LNode 2 Empty)))
--     ==> LNode 2 (LNode 3 (LNode 4 Empty))

-- Ответ:
data List a = Empty | LNode a (List a)
  deriving (Show)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f Empty = Empty
  fmap f (LNode head tail) = LNode (f head) (fmap f tail)

instance Applicative List where
  pure :: a -> List a
  pure x = LNode x Empty
  (<*>) :: List (a -> b) -> List a -> List b
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  (LNode f fs) <*> xs = append (fmap f xs) (fs <*> xs)
    where
      append Empty ys = ys
      append (LNode x xs) ys = LNode x (append xs ys)

instance Monad List where
  return :: a -> List a
  return = pure
  (>>=) :: List a -> (a -> List b) -> List b
  (>>=) Empty _ = Empty
  (>>=) (LNode x xs) f = concat'' (f x) (xs >>= f)
    where
      concat'' :: List a -> List a -> List a
      concat'' l Empty = l
      concat'' Empty l = l
      concat'' (LNode x xs) other = LNode x (concat'' xs other)

------------------------------------------------------------------------------
-- Определить экземпляры классов типов Functor, Applicative и Monad для типа
-- данных Fun.
--
-- Подсказка: попробовать реализовать сначала следующие экземпляры классов типов:
--                instance Functor ((->) a) where
--                …
--                instance Applicative ((->) a) where
--
--            Чтобы понять какие реализации должны быть в этом случае у функций
--            fmap, pure и (<*>)
--            можно выписать их типы.
--
--            Здесь (->) a b = a -> b, т.е. ((->) a)::*->* – это частично
--            применённый конструктор типа (->)::*->*->*.
--
--            В модуле Prelude, который подключается при загрузке интерпретатора,
--            указанные реализации уже есть, поэтому предлагается проделать это
--            вспомогательное упражнение без загрузки и проверки результатов в
--            интерпретаторе, а только как наводящее
--            упражнение для решения задачи.

-- Ответ:
newtype Fun a b = Fun {getFun :: a -> b}

instance Functor (Fun a) where
  fmap :: (a2 -> b) -> Fun a1 a2 -> Fun a1 b
  fmap f (Fun g) = Fun (f . g)

instance Applicative (Fun a) where
  pure :: a2 -> Fun a1 a2
  pure x = Fun (const x)
  (<*>) :: Fun a1 (a2 -> b) -> Fun a1 a2 -> Fun a1 b
  (Fun f) <*> (Fun g) = Fun (\x -> f x (g x))

instance Monad (Fun a) where
  return :: a2 -> Fun a1 a2
  return = pure
  (>>=) :: Fun a1 a2 -> (a2 -> Fun a1 b) -> Fun a1 b
  (Fun f) >>= g = Fun (\x -> getFun (g (f x)) x)

------------------------------------------------------------------------------
-- Упр. 16. Докажите, что любая монада – это также функтор и аппликативный
-- функтор.
-- Указание: для выполнения задания необходимо, используя функции:
--              return :: a -> m a
--              (>>=)  :: m a -> (a -> m b) -> m b
-- Реализовать следующие функции:

-- Ответ:
fmap' :: (Monad m) => (a -> b) -> m a -> m b
fmap' f ma = ma <&> f

pure' :: (Monad m) => a -> m a
pure' = return

ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' mf ma = mf >>= (ma <&>)

------------------------------------------------------------------------------
-- Упр. 17. Напишите выражение, которое печатает в консоль “Hello world!”.

-- Ответ:
expression17 :: IO ()
expression17 = putStrLn "Hello world!"

------------------------------------------------------------------------------
-- Упр. 18. Напишите функцию, которая запрашивает из консоли "имя", а затем
--         печатает в консоль:
--              “Good day, имя”

-- Ответ:
expression18 :: IO ()
expression18 = do
  name <- getLine
  putStrLn $ "Good day, " ++ name

------------------------------------------------------------------------------
-- Упр. 19. Реализуйте цикл while. while должен выполнять операцию до тех пор,
-- пока условие возвращет значение True.
--
-- Примеры:
--   -- ничего не печатает
--   while (return False) (putStrLn "IMPOSSIBLE")
--
--   -- печатает YAY! до тех пор пока пользователь продолжает вводить Y.
--   while ask (putStrLn "YAY!")

-- Ответ:
ask :: IO Bool -- используется в примере
ask = do
  putStrLn "Y/N?"
  line <- getLine
  return $ line == "Y"

while :: IO Bool -> IO () -> IO ()
while cond op = do
  condition <- cond
  if condition then op >> while cond op else pure ()

------------------------------------------------------------------------------
