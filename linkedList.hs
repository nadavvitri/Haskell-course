-- 2)
data LinkedList a = Node a (LinkedList a) | Null deriving (Show, Eq, Ord)

add :: a -> LinkedList a -> LinkedList a
add x Null =  Node x Null
add x (Node a linkedlist) = Node a (add x linkedlist)

toList :: LinkedList a -> [a]
toList Null = []
toList (Node a linkedlist) = a : toList (linkedlist)

--toList :: [a] -> LinkedList a
toLinkedList [] = Null
toLinkedList (x:xs) = add x (toLinkedList xs)

