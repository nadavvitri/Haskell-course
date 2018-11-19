data Stack a = Node a (Stack a) | Null deriving (Show, Eq)

push :: a -> Stack a -> Stack a
push x stack = Node x stack

pop :: Stack a -> Maybe (Stack a, a)
pop Null = Nothing
pop (Node a rest) = Just (rest, a)

peek :: Stack a -> Maybe a
peek Null = Nothing
peek (Node x rest) = Just x
