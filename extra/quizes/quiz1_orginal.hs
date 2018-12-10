------------ q1 -----------------
data Tree a = Node [Tree a] | R | B deriving (Show, Eq)

------------ q2 -----------------
let a = Node [R, B, B]
let b = Node [R]
let c = Node [R, R]
let d = Node [B]
let tree = Node [a,b,c,d]

------------ q3 -----------------
eval :: Tree a -> Tree a
eval B = B
eval R = R
eval (Node xs)
    | (count R (Node xs)) > (count B (Node xs)) = R
    | (count B (Node xs)) > (count R (Node xs)) = B
    | otherwise = error "Error!"

count :: Tree a -> Tree a -> Int
count R R = 1
count R B = 0
count B B = 1
count B R = 0
count a (Node xs) = sum (map (count a) xs)
