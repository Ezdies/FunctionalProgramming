-- map, filter, all, any, fold*, sum, zipWith
-- all dla listy pustej każdy spełnia
-- any dla listy pustej zawsze false
-- !!, ++, null, head, tail
-- length

poprawnaMacierz :: [[a]] -> Bool
poprawnaMacierz [] = False
poprawnaMacierz ([] : _) = False
poprawnaMacierz (h:t) =
    let
        lKol = length h
    in
    --    all(\w -> length w == lKol) t
        all ((== lKol) . length) t
        -- \x -> (==) lKol (length x)
      --  (== lKol) . length

elementMacierzy :: [[a]] -> Int -> Int -> a
elementMacierzy mac m n = (mac !! m) !! n

sumaElementow :: Num a => [[a]] -> a
--sumaElementow mac = sum (map sum mac)
sumaElementow = sum . map sum

macierzStala :: a -> Int -> Int -> [[a]]
macierzStala wart m n = replicate m (replicate n wart)

macierzZerowa :: Num a => Int -> Int -> [[a]]
-- macierzZerowa m n = macierzStala 0 m n
macierzZerowa = macierzStala 0

wierszMacJedn :: Num a => Int -> Int -> [a]
wierszMacJedn n i =
    replicate i 0 ++ [1] ++ replicate (n - i - 1) 0

macierzJednostkowa :: Num a => Int -> [[a]]
macierzJednostkowa n =
   -- map (\i -> wierszMacJedn n i)[0..n-1]
   map (wierszMacJedn n) [0..n-1]

-- przy tablicach dwuwymiarowych trzeba robić referencje do wierszy gdzie nic nie zmieniamy i kolumny po zmienionej wartości


zmianaEl :: Int -> a -> [a] -> [a]
zmianaEl 0 w (_:t) = w : t
zmianaEl n w (h:t) = h : zmianaEl (n-1) w t


zmianaElMac :: Int -> Int -> a -> [[a]] -> [[a]]
zmianaElMac m n w mac =
    zmianaEl m (zmianaEl n w (mac !! m)) mac

kolumna :: Int -> [[a]] -> [a]
--kolumna n mac = map(\w -> w !! n) mac
--kolumna n = map(\w -> w !! n)
kolumna n = map (!! n)

przekatna :: [[a]] -> [a]
przekatna mac = zipWith (!!) mac [0..]

sumaMac :: Num a => [[a]] -> [[a]] -> [[a]]
-- sumaMac m1 m2 = zipWith(zipWith (+)) m1 m2
sumaMac = zipWith (zipWith (+))