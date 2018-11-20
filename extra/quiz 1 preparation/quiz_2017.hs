data TreeColor a = Node a [TreeColor a] | Null deriving (Show)

isGrey :: TreeColor String -> Bool
isGrey Null = False
isGrey tree = (elem' "Red" tree) && (elem' "Green" tree) && (elem' "Blue" tree)

elem' :: String -> TreeColor String -> Bool
elem' _ Null = False
elem' c (Node a xs)
    | c == a = True
    | otherwise = or $ map (elem' c) xs 

map1 :: (a -> b) -> [a] -> [b]
map1 _ [] = []
map1 f (x:xs) = (f x) : (map1 f xs)

map2 f xs = foldr (\x acc -> (f x) : acc) [] xs

map3 f xs = foldl (\acc x -> acc ++ [f x]) [] xs
