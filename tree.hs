data Tree a = Node a [Tree a] | Null deriving (Eq, Show, Ord)

elem' :: (Eq a) => a -> Tree a -> Bool
elem' y Null = False
elem' y (Node a xs)
    | y == a = True
    | otherwise = foldl (\acc x -> if (elem' y x) == True then True else acc) False xs 

elem'' y Null = False
elem'' y (Node a xs)
    | y == a = True     
    | otherwise = or $ map (elem'' y) xs

sum' :: (Num a) => Tree a -> a
sum' Null = 0
sum' (Node x xs) = x + sum (map sum' (xs))
