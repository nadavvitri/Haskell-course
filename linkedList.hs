-- 2)data Node = Next | Value deriving (Show)
data LinkedList = Node Int Node | Null

add :: Integral a => LinkedList a-> LinkedList 
add (Null a) = 
