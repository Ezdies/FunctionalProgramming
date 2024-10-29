empty :: [a]
empty = []

odd_numbers_hpl n = n : (-n) : odd_numbers_hpl(n + 2)
odd_numbers :: Integral a => [a]
odd_numbers = odd_numbers_hpl 1

even_numbers :: Integral a => [a]
even_numbers = 0 : odd_numbers_hpl 2

contains :: Eq a => [a] -> a -> Bool
--containts s x = elem x s
contains = flip elem

add :: Eq a => a -> [a] -> [a]
add x s = 
    if contains x s then s 
    else x : s 


remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (h:t)
--  |x == h    = remove x t - jeżeli zakładamy wiele wystąpień w liście
    | x == h    = t
    | otherwise = h : remove x t


sum' :: Eq a => [a] -> [a] -> [a]
sum' s1 s2 = s1 ++ filter (\x -> not (contains s1 x)) s2 
-- sum' s1 s2 = s1 ++ filter (not . (contains s1)) s2 

intersection :: Eq a => [a] -> [a] -> [a]
intersection s1 s2 = filter(contains s1) s2

difference :: -- to samo co w sum tylko zamieniamy kolejnością s1 i s2
difference = undefined

complement ::
complement = undefined