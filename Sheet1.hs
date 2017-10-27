sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

not' :: Bool -> Bool
not' b 
    | b = False
    | otherwise = True

not'' :: Bool -> Bool
not'' b = if b then False else True

not''' :: Bool -> Bool
not''' True = False
not''' False = True

not'''' :: Bool -> Bool
not'''' b = fromInt . (`mod` 2) . (+1) . toInt $ b
    where   fromInt :: Integer -> Bool
            fromInt 0 = False
            fromInt 1 = True
            toInt False = 0
            toInt True = 1

implies a b = (!a) || b 

