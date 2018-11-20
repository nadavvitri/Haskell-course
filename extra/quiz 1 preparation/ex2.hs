data Binary a = Zero (Binary a) | One (Binary a) | Null deriving (Show)

and' :: Binary a -> Binary a -> String
and' _ Null = "0"
and' Null _ = "0"
and' (One xs) (One ys) = (and' xs ys) ++ "1"
and' (One xs) (Zero ys) = (and' xs ys) ++ "0"
and' (Zero xs) (One ys) = (and' xs ys) ++ "0"
and' (Zero xs) (Zero ys) = (and' xs ys) ++ "0"
