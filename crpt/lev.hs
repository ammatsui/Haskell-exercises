lev :: String -> String -> Int
lev []     b      = length b
lev a      []     = length a
lev (a:as) (b:bs) = if (a == b) then (lev as bs) else 1 + min $ (lev as (b:bs)) (lev (a:as) bs) (lev as bs)
