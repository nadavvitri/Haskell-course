-- 1)
import Data.List

lookAndSay ::(Eq a, Num a, Ord a) => a -> String
lookAndSay 1 = "1"
lookAndSay n = if (n <= 0) then "Error" else g (map (\xs -> show (length xs) ++ [head(xs)]) (group (lookAndSay (n-1))))
     where g xs = foldl (\acc x -> acc ++ x) "" xs
