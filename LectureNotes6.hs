sort' :: Ord a=> [a] -> [a]
sort' xs = head . filter sorted . permutations $ xs

sorted :: Ord a=> [a] -> Bool
sorted (x:y:xs) = x<=y && sorted(y:xs)
sorted _ = True

permutations [] = [[]]
permutations xs = [x:ys | x <- xs, ys <- permutations (exclude x xs)]

exclude x xs = takeWhile (/=x) xs ++ (tail.dropWhile (/=x) $ xs)

-- | all permutations of the tail, insert head at all possible positions

permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' (x:xs) = concat . map (insertAtAll x) . permutations' $ xs

insertAtAll x xs = [insertAt i x xs | i <- [0..length xs]]
insertAt i x xs = take i xs ++ (x:drop i xs)


permutations'' :: [a] -> [[a]]
permutations'' [] = [[]]
permutations'' (x:xs) = [ zs | ys <- permutations'' xs, zs <- include x ys ]
include :: a -> [a] -> [[a]]
include x [] = [[x]]
include x (y:ys) = [x:y:ys] ++ map (y:) (include x ys)

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)

insert x xs = takeWhile (<x) xs ++ (x:dropWhile (<x) xs)

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = minimum xs : (selectionSort (exclude (minimum xs) xs))

-- | foldr :: (b -> a -> b) -> b -> [a] -> b
-- | foldr _ b [] = b
-- | foldr f b (x:xs) = f x (foldr f b xs)
-- | 
-- | foldl :: (a -> b -> a) -> a -> [b] -> a
-- | foldl f b []     = b
-- | foldl f b (x:xs) = foldl f (f b x) xs

minimum' (x:[]) = x
minimum' (x:xs) = min x (minimum' xs)

minimum'' :: Ord a => [a] -> a
minimum'' = foldr1 (\ x a -> min x a)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =  quicksort   [a | a <- filter (<x) xs] ++ 
                    x:          [a | a <- filter (==x) xs] ++
                    quicksort   [a | a <- filter (>x) xs]

mergesort :: Ord a => [a] -> [a]
mergesort (x:[]) = [x]

mergesort xs = merge (mergesort ls) (mergesort rs)
        where (ls, rs) = halves xs

merge [] xs = xs
merge xs [] = xs
merge (a:as) (b:bs) | a<=b = a:b:(merge as bs)
                    | otherwise = b:a:(merge as bs)

halves xs = splitAt (length xs `div` 2) xs