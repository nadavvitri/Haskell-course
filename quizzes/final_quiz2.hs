import Test.QuickCheck
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Control.Monad.Fix
import Control.Monad.State

------------ q1 -----------------
newtype Parser a = P (String -> [(a, String)])

doParse :: Parser a -> String -> [(a, String)]
doParse (P p) s = p s

toTake :: Char -> Bool
toTake '-' = True
toTake c = if (length ([i|i<-[0..9], show (i) == [c]]) > 0) then True else False

split = P (\s -> case s of
                 [] -> []
                 s -> [( (takeWhile toTake s), (dropWhile toTake s))])

final :: Parser Int
final = P $ (\s -> let [(sv, r)] = doParse split s in
                      if (head sv) == '-' then [(convert (tail sv) * (-1), r)]
                      else [(convert sv, r)])

--added
convert :: String -> Int
convert = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1
        
------------ q2 -----------------
frac = fix p
     where p f x = if (x == 0) then 1 else x * (f (pred x))

------------ q3 -----------------
modify1 :: (a -> a) -> State a ()
modify1 f = state $ (\s -> ( (), (f s) ))


{-
Changes:
q1) I didn't remember exactly the names of the functions. So string to int and int to binary 
changes to convert (I ask during the exam because I have no time to write this function but Michael told me 
this is not the main porpose of the question so I can assume I have basic & library functions and continue work on the Parser).
Also I had some small syntax error ("in" and then if & else)
q2) Small syntax error (forgot "where")
q3) Michael told us that we can assume f is from Int -> Int. but this can't 
work with (\). So I generallized the signature.

-- I think my grade should be 98 because the mistake in q1 was minor (-2 point).

-- I always coming to the lectures and doing homework :) !
-}

-- QuickCheck OK, passed 100 tests. --
prop_CountersEqual = 
  forAll binString $ \s -> let (x, y) = head (doParse final s) in
                               if (x > 0) then (showIntAtBase 2 intToDigit (x) "") ++ y == s
                               else (if (x == 0) then  (showIntAtBase 2 intToDigit (x) "") ++ y == (s)
                                     else "-" ++ (showIntAtBase 2 intToDigit (x * (-1)) "") ++ y == s)

binString :: Gen String
binString = liftM2 (:) (elements "11") (liftM2 (++) (vectorOf 50 (elements "01")) (listOf $ elements ['a'..'z']))
