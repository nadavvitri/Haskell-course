


-- 3)
elem1 ::(Eq a) => a -> [a] -> Bool
elem1 a [] = False
elem1 a (x:xs)
    | a == x = True
    | otherwise = elem1 a xs


elem2 :: (Eq a) => a -> [a] -> Bool   
elem2 a xs = foldl (\acc x -> if (a == x) then True else acc) False xs


elem3 :: (Eq a) => a -> [a] -> Bool
elem3 a xs = (\ys -> if ys == [] then False else True) [x | x <- xs, a == x] 
