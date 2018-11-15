-- 2)
data Tree a = Plus (Tree a) (Tree a)
             | Sub (Tree a) (Tree a) 
             | Div (Tree a) (Tree a) 
             | Mul (Tree a) (Tree a) 
             | Pow (Tree a) (Tree a)
             | Sin (Tree a)
             | Cos (Tree a)
             | Const (Maybe a) 
             | X deriving (Show, Eq)


eval :: (Floating a) => Tree a -> Maybe a -> Maybe a
eval X x = x
eval (Const value) x = value
eval (Plus left right) x = (+) <$> (eval left x) <*> (eval right x)
eval (Sub left right) x = (-) <$> (eval left x) <*> (eval right x)
eval (Div left right) x = (/) <$> (eval left x) <*> (eval right x)
eval (Mul left right) x = (*) <$> (eval left x) <*> (eval right x)
eval (Pow left right) x = (**) <$> (eval left x) <*> (eval right x)
eval (Sin left) x = (sin) <$> (eval left x)
eval (Cos left) x = (cos) <$> (eval left x)


differentiate :: (Fractional a) => Tree a -> Tree a
differentiate (Const value) = Const (Just 0.0)
differentiate X =  Const (Just 1.0)
differentiate (Plus left right) = Plus (differentiate left) (differentiate right)
differentiate (Sub left right) = Sub (differentiate left) (differentiate right)
differentiate (Mul left right) = Plus (Mul (differentiate left) right) (Mul left (differentiate right))
differentiate (Div left right) = differentiate (Mul left (Pow right (Const (Just (-1.0)))))
differentiate (Pow left right) = Mul right (Pow left (Sub right (Const (Just 1.0))))
differentiate (Cos left) = Mul (Sub (Const (Just 0.0)) (Sin left)) (differentiate left)
differentiate (Sin left) = Mul (Cos left) (differentiate left)





