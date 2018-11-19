--1)
myLast :: [a] -> a
myLast [] = error "Empty!"
myLast xs = xs !! (length (xs) - 1)

--2)
myButLast :: [a] -> a
myButLast [] = error "Empty!"
myButLast (x:[]) = error "Only one item.."
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

--3)
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty!"
elementAt (x:xs) 1 = x
elementAt (x:xs) i = elementAt xs (i-1)

elementAt' xs i
         | (i < 1) || (i > length (xs)) = error "Error!"
         | otherwise = last $ (take i xs)

--4)
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

myLength' xs = foldl (\acc x -> acc + 1) 1 xs
