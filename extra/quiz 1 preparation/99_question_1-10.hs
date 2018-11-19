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

--5)
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse (xs) ++ [x]

myReverse' xs = foldl (\acc x -> x:acc) [] xs

--6)
isPal :: (Eq a) => [a] -> Bool
isPal [] = True
isPal [a] = True
isPal xs
    | (head (xs) == last (xs)) = isPal (tail (init xs))
    | otherwise = False

--7)
data NestedList a = Elem a | List [NestedList a] deriving (Show)

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List xs) = f (map (flatten) xs)
       where f ys = foldl (\acc x -> acc ++ x) [] ys

--8)
cmprs :: (Eq a) => [a] -> [a]
cmprs [] = []
cmprs (y:xs) = foldl (\acc x -> if ((last acc) /= x) then (acc ++ [x]) else acc) [y] xs

--9)
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x: takeWhile (==x) xs) : (pack (dropWhile (==x) xs))

--10)
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = ( (1 + length (takeWhile (==x) xs)), x) : encode (dropWhile (==x) xs)

encode' xs = map (\x -> (length x, head x)) (pack xs)
