import Distribution.Simple.Utils (xargs)
-- map -przekszdałcanie każdej wartości według warunku, filter- filtrowanie według danego predykatu
-- take, wzięcie x pierwszych elementów
-- drop - pozbycie się pierwszych x elementów
-- takeWhile - bierze elementy dopóki jest spełniony predykat
-- dropWhile - usuwa pierwsze jeśli jest spełnionqy predykat
-- !! operator indexowania [2,3,4,5] !! 3 to 5
-- [2,3,4] ++ [5,6,7] to [2,3,4,5,6,7] - sklejanie dwóch list złożoność to O(N), gdzie długość
-- replicate 3 4 [4,4,4]
-- repeat 4 - nieskończone 4
-- cycle [1,2,3] -- kręci się w nieskończoność
-- zip [1,2,3] ['a', 'b', 'c', 'd'] - tworzy listę par i kończy się tam gdzie krótsza lista
-- zipWith - bierze elementy z każdej listy oraz funkcje i tworzy listę wynikową
-- zipWith (\x y -> 10 * x + y [1,2,3][5,4,8])
-- foldl, folde, foldl1, foldr1
-- foldl - funkcja to pojedynczy krok obliczeń, bierze element początkowy i bierze pierwszy element z listy, potem f z tego co jest wzięte i bierzemy kolejny element
-- foldl (((0+1)+2)+3+4)
-- foldr, to samo, tylko od prawej (1+(2+(3+(4+0))))
-- foldl1 - bierze pierwszy element z listy, i muszą się typy wartości zgadzać
-- foldl (*) 1 [1,2,3]
-- foldr (:)[][1,2,3] to [1,2,3]
-- data Maybe a = Nothing | Just a

-- f :: Maybe a -> a
--f (Just x) = x

digitsToNumber1 :: Integral a => [a] -> a
digitsToNumber1 = 
     foldl (\number digit -> 10 * number + digit) 0


digitsToNumber2 :: Integral a => [a] -> a
digitsToNumber2 = 
     foldl1 (\number digit -> 10 * number + digit)


-- removeFromEnd x lst = reverse(dropWhile(== x)(reverse lst))
--removeFromEnd x = reverse . dropWhile(== x) . reverse

removeFromEnd2 :: (Eq a) => a -> [a] -> [a]
removeFromEnd2 x  = foldr (rfeStep x) []

rfeStep x el [] = 
    if el == x then []
    else [el]
rfeStep _ el lst = el:lst

simpleFraction :: Fractional a => [a] -> a
simpleFraction = 
    foldr1 (\x y -> x + 1/y)

sumOfMaybe1 :: Num a => [Maybe a] -> a
sumOfMaybe1 [] = 0
sumOfMaybe1 (Nothing:t) = sumOfMaybe1 t
sumOfMaybe1 ((Just x):t) = x + sumOfMaybe1 t 

sumOfMaybe2 :: Num a => [Maybe a] -> a
--sumOfMaybe2 [] = 0
--sumOfMaybe2 (h:t) = valueOfMaybe h + sumOfMaybe1 t
sumOfMaybe2 = sum . map valueOfMaybe


valueOfMaybe (Just x) = x
valueOfMaybe Nothing = 0

add :: (Num a) => a -> Maybe a -> a
add x Nothing = x
add x (Just y) = x + y 

sumOfMaybe3 :: Num a => [Maybe a] -> a
sumOfMaybe3 = foldl add 0 

maybeSumOfMaybe :: Num a => [Maybe a] -> Maybe a
maybeSumOfMaybe [] = Just 0
maybeSumOfMaybe(Nothing:_) = Nothing
maybeSumOfMaybe(Just x: t) =
    case maybeSumOfMaybe t of
        Nothing -> Nothing
        Just v -> Just (v + x)
        
valueOfJust (Just x) = Just x
valueOfJust Nothing = Nothing

