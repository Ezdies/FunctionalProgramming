-- map przekształca każdy element listy na elementy w innej liście
--functor to coś co ma określany typ jak dostanie parametr, który go określa
--functor zachowuje strukturę czegoś, ale operuje na elementach
-- <$ zachowuje strukturę i zwraca to samo wszędzie czyli 3 <$

data BTree a = E | L a | N (BTree a) (BTree a)
    deriving Show

t1 = N
        (N (L 50) (L 20))
        (N (N (L 3) (L 5)) (N (L 6) E))


instance Functor BTree where
    fmap _ E = E -- dopasowanie wzorca na kolejnym przypadku, zaczynamy od najłatwiejszego przypadku
    fmap f (L val) = L (f val)
    fmap f (N left right) =
        N (fmap f left) (fmap f right)


--jak napiszemy foldr, to automatycznie dodawany jest na początek element więc można zamienić drzewo na listę
-- foldr (:)[] tree -- wszystkie elementy dodawane są do akumulatora, i zaczynamy tutaj dla listy pustej

instance Foldable BTree where
    foldr _ acc E = acc
    foldr f acc (L val) = f val acc
    foldr f acc (N left right) =
        foldr f (foldr f acc right) left
        -- let 
        --     acc' = foldr f acc right
        -- in 
        --     foldr f acc' left

data Tree a = Node a [Tree a]
    deriving Show

t2 = Node 10
    [Node 5 [Node 3 [], Node 2 []] ,Node 4 [Node 1 []], Node 8 [], Node 0 []]

instance Functor Tree where
    fmap f (Node val trees) =
        Node (f val) (fmap (fmap f) trees)
        -- Node(f val) (fmap(\tree -> fmap f tree) trees) 

instance Foldable Tree where
    foldr f acc (Node val trees) =
        f val (foldr (\tree a -> foldr f a tree) acc trees)
