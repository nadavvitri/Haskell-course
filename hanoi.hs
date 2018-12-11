import Control.Monad.Writer

data Tower = Tower String String String deriving (Show, Eq)

solve :: Tower -> Writer [String] Tower
solve (Tower src aux dst) = solve' (length(src)) (Tower src aux dst)

solve' :: Int -> Tower -> Writer [String] Tower
solve' 1 (Tower src aux dst) = do
           tell ["Moving " ++ [last src] ++ " to " ++ dst]
           return (Tower (init src) aux (dst ++ [last src]))
solve' n (Tower src aux dst) = do
           (Tower x y z)    <- solve' (n - 1) (Tower src dst aux)
           (Tower x2 y2 z2) <- solve' 1 (Tower x z y)
           (Tower x3 y3 z3) <- solve' (n - 1) (Tower y2 x2 z2)      
           return (Tower y3 x3 z3)
{-
2 "21" "" ""
    1 "21" "" ""
        "2" "" "1" *moved*
1 "2" "1" ""
        "" "1" "2" *moved*
1 "1" "" "2"
        "" "" "21" *moved*
-}
