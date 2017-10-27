transpose' :: [[a]] -> [[a]]

transpose' [] = []
transpose' [[]] = []
transpose' rows = map head (filter (not . null) rows) : transpose' (map tail (filter (not . null) rows))

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' count (x:xs) = drop' (count-1) xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' count (x:xs) = x : take' (count-1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' count xs = (take' count xs, drop' count xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) 
            | p x = x : takeWhile' p xs
            | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
            | p x = dropWhile' p xs
            | otherwise = x:xs

span' p xs = (takeWhile' p xs, dropWhile' p xs)

break' p = span (not . p)