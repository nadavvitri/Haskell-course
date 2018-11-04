-- 4)
subList :: (Enum a, Num a, Ord a) => a -> a -> [b] -> [b]
subList i j xs = if ((i < 0) || (j < 0)) then [] else (f (zip' [0..] xs))
       where f ys = [y | (x,y)<-ys, x >= i, x <= j]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y): zip' xs ys
