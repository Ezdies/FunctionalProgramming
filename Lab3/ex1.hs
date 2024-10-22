import Distribution.Simple.Utils (xargs)
import Data.IntMap (difference)
import Data.Bits (Bits(complement))
empty :: a -> Bool
empty _ = False

odd_numbers :: (Integral a) => a -> Bool
odd_numbers = odd

even_numbers :: (Integral a) => a -> Bool
even_numbers = even

contains :: (a-> Bool) -> (a -> Bool)
--contains s = s
contains = id

add :: (Eq a) => (a -> Bool) -> a -> (a -> Bool) -- funkcja dwuargumentowa zwracająca funkcję dwuargumentową zwracającą bool
-- \x to jest lambda
add s el = \x -> s x || x == el
-- add s el x = s x || x == el // lambdę można przenieść jako argument funkcji

-- (add s 6) 6 //sprawdziamy czy dla 6 zwróci nam true
-- (\x -> s x || x == 6) 6
-- to jest funkcja która dostaje x  i zwraca takie wyrażenie. Nie wiemy jaką ma wartość, ale widać, że to jest true
-- sprawdzamy czy należy do zbioru lub jest to ostatnia liczba dodana do zbioru


remove :: (Eq a) => (a -> Bool) -> a -> (a -> Bool)
-- sprawdzamy czy element należy do oryginalnego zbioru i nie jest elementem usuwanym
remove s el x = s x  &&  x /= el 

sum' :: (a -> Bool) -> (a -> Bool) -> (a -> Bool) -- dwa argumenty funkcji i wartość zwracana
sum' s1 s2 x  = s1 x || s2 x-- mamy dwa zbiory i argument x. Wynikiem będzie dodanie go do jednego lub drugiego zbioru

intersection :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
intersection s1 s2 x = s1 x && s2 x

difference' :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
difference' s1 s2 x = s1 x && not (s2 x)

complement' :: (a -> Bool) -> (a -> Bool)
complement' s x = not (s x) -- to jest funkcja jednoargumentowa, ale w wywołaniu ma s i x, bo ten x jest jako zamiennik lambdy czyli zamiast \x gdyby było to jednoargumentowe
-- complement' s = not . s

list_2_predicate :: (Eq a) =>  [a] -> (a -> Bool) -- zwraca Boola wszystko bo to mają być predykaty
-- list_2_predicate xs x = elem x xs
list_2_predicate = flip elem  