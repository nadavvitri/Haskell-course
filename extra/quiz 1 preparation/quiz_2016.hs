length1 :: [a] -> Int
length1 [] = 0
length1 (x:xs) = 1 + (length1 xs)

length2 xs = foldr (\x acc -> acc + 1) 0 xs

data DivTree a = Div (DivTree a) (DivTree a) | Leaf a deriving (Show)

eval' :: DivTree Double -> Maybe Double
eval' (Leaf a) = Just a
eval' (Div left right) 
    | (eval' right == 0) = Nothing
    | otherwise = (/) <$> (eval' left) <*> (eval' right)
