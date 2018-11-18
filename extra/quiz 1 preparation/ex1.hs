group :: (Eq a) => [a] -> [[a]]
group [] = []
group (x:xs) = (x : takeWhile (== x) xs) : group (dropWhile (== x) xs)
