group :: (Eq a) => [a] -> [[a]]
group [] = []
group (x:xs) = (x : takeWhile (== x) xs) : group (dropWhile (== x) xs)


data BoolTree a = And (BoolTree a) (BoolTree a) 
                | Or (BoolTree a) (BoolTree a) 
                | Not (BoolTree a) 
                | Leaf a deriving (Show, Eq)

eval :: BoolTree Bool -> Bool
eval (Leaf a) = a
eval (And left right) = (eval left) && (eval right)
eval (Or left right) = (eval left) || (eval right)
eval (Not left) = not (eval left)
