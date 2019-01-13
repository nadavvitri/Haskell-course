import Test.QuickCheck

------------ q1 -----------------
data Tree a = Node [Tree a] | R | B deriving (Show, Eq)

------------ q2 -----------------
a = Node [R, B, B]
b = Node [R]
c = Node [R, R]
d = Node [B]
tree = Node [a,b,c,d]

------------ q3 -----------------
eval :: Tree a -> Tree a
eval B = B
eval R = R
eval (Node xs)
    | count > 0 = R
    | count < 0 = B
    | otherwise = error "Error!"
    where count = foldr (\x acc -> if (eval x) == R then acc + 1 else acc - 1) 0 xs


--changes:
--q2: removed let in q2 (I thought we need to write like in the ghci)
--q3: I misunderstood the question, I thought we needed to count the total number of leaves and answer according to the majority. Later on, during the quiz you drew two examples but I already finished writing the test and I had to go. I had to change to count the majotiry according to each node seperately

-- I think my grade should be 90 because the mistake in q2 was minor (-1 point). 
-- And for q3 I misunderstood the logic well (-9 points)

-- I always coming to the lectures and doing homework :) !

instance Arbitrary (Tree a) where 
 arbitrary = sized arbitrarySizedTree

arbitrarySizedTree :: Int -> Gen (Tree a)
arbitrarySizedTree m = do
   n <- choose (0, m `div` 2)
   ts <- vectorOf n (arbitrarySizedTree (m `div` 4))
   case n of
     0 -> oneof [return R]
     _ -> return (Node [Node ts])


prop_Monotonous :: Tree a -> Bool
prop_Monotonous tree
    | (eval tree == R) = eval (Node [tree, R]) == R
    | (eval tree == B) = eval (Node [tree, B]) == B


