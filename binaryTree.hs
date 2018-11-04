-- 5)
data Tree a = Node a (Tree a) (Tree a) | Null deriving (Eq, Ord, Show)

--add :: (Eq a, Ord a) => a -> Tree a -> Tree a
add x Null = Node x Null Null
add x (Node a left right)
   | x == a = Node x left right
   | x < a = Node a (add x left) right
   | x > a = Node a left (add x right)


--elem' :: (Eq a) => a -> Tree a -> Bool
elem' x Null = False
elem' x (Node a left right)
    | x == a = True
    | x < a = elem' x left
    | x > a = elem' x right

--sum' :: Tree a -> Integral
sum' Null = 0
sum' (Node a Null right) = a + sum' right
sum' (Node a left Null) = a + sum' left
sum (Node a left right) = a + (sum' left) + (sum' right)
