import Data.Maybe

-- | Q 5.1 

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x:(take' (n-1) xs)

-- | take' 4 [1..]
-- | [1,2,3,4]

-- | take' 4 [1,2]
-- | [1,2]

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs

-- | drop' 3 [1..10]
-- | [4,5,6,7,8,9,10]

-- | drop' 3 [1]
-- | []

-- | TODO find definition of strict

-- | 5.2 Is map strict? Is map f strict?
-- | TODO find definition of strict

-- | 5.3

evens :: [a] -> [a]
evens (x:_:xs) = x:evens xs
evens (x:xs) = x:[]
evens _ = []

odds (_:xs) = evens xs
odds [] = []

-- | Def without evens:
odds' :: [a] -> [a]
odds' (_:x:xs) = x:odds xs
odds' _ = []

alternates :: [a] -> ([a],[a])
alternates [] = ([],[])
alternates [x] = ([x],[])
alternates (x:y:xs) = (x:a, y:b) 
    where (a, b) = alternates xs

-- | *Main> alternates [1..10]
-- | ([1,3,5,7,9],[2,4,6,8,10])
-- | (0.01 secs, 143,272 bytes)

-- | *Main> alternates [1..11]
-- | ([1,3,5,7,9,11],[2,4,6,8,10])
-- | (0.00 secs, 142,736 bytes)

-- | Deriving alternates from
-- | > alternates :: [a] -> ([a],[a])
-- | > alternates xs = (evens xs, odds xs)

-- | The base cases:
-- | alternates [] = (evens xs, odds xs) = ([],[])
-- | alternates [x] = (evens [x], odds [x]) = ([x],[])
-- | 
-- | Deriving from (evens xs, odds xs):
-- | alternates xs       = (evens xs, odds xs)
-- | alternates (x:y:xs) = (evens (x:y:xs), odds (x:y:xs))
-- |                     = (x:evens xs, y:odds xs)
-- |                     = (x:a, y:b) where (a, b) = (evens xs, odds xs)
-- |                     = (x:a, y:b) where (a, b) = alternates xs


-- | 6.1 

-- | TODO find out the definition of strict
-- | curry' :: ((a,b) -> c) -> a -> b -> c
-- | curry' f a b = f (a,b)
-- | uncurry' :: (a -> b -> c) -> (a, b) -> c
-- | uncurry' f (a,b) = f a b
-- | 
-- | (curry . uncurry) f a b     = curry ( uncurry f ) a b
-- |                             = (uncurry f) (a, b)
-- |                             = uncurry f (a, b)
-- |                             = f a b
-- | curry.uncurry :: (a -> b -> c) -> (a -> b -> c)
-- | 
-- | (uncurry . curry) f (a,b)   = uncurry ( curry f ) (a,b)
-- |                             = (curry f) a b
-- |                             = curry f a b
-- |                             = f (a,b)
-- | uncurry.curry :: ((a,b) -> c) -> ((a,b) -> c)
-- | 
-- | Which shows that (curry.uncurry) and (uncurry.curry) are identity functions,
-- | and that curry and uncurry are mutually inverse, since the definition inverse is:
-- | g is the inverse of f iff f.g is an identity function on the type of the parameter of g.

-- | 6.2 

zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y):zip' xs ys
zip' [] _ = []
zip' _ [] = []

-- | *Main> zip [1..3] ['a'..'z']
-- | [(1,'a'),(2,'b'),(3,'c')]
-- | *Main> zip' [1..3] ['a'..'z']
-- | [(1,'a'),(2,'b'),(3,'c')]

-- | 6.3

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f as bs = map (uncurry f) $ zip as bs

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f (x:xs) (y:ys) = f x y : zipWith'' f xs ys
zipWith'' f [] _ = []
zipWith'' f _ [] = []

-- | *Main> zipWith (\ a b -> [a,b]) ['a'..'z'] ['A'..'H']
-- | ["aA","bB","cC","dD","eE","fF","gG","hH"]
-- | *Main> zipWith' (\ a b -> [a,b]) ['a'..'z'] ['A'..'H']
-- | ["aA","bB","cC","dD","eE","fF","gG","hH"]
-- | *Main> zipWith'' (\ a b -> [a,b]) ['a'..'z'] ['A'..'H']
-- | ["aA","bB","cC","dD","eE","fF","gG","hH"]

zip'' :: [a] -> [b] -> [(a,b)]
zip'' = zipWith'' (\ x y -> (x, y))

-- | *Main> zip [1..3] ['a'..'z']
-- | [(1,'a'),(2,'b'),(3,'c')]
-- | *Main> zip'' [1..3] ['a'..'z']
-- | [(1,'a'),(2,'b'),(3,'c')]

-- | 6.4

splits :: [a] -> [(a,[a])]
splits [] = []
splits (x:xs) = (x,xs) : (map (\ (k, ys) -> (k, x:ys)) . splits $ xs)

-- | using unfold:

-- | Trying to come up with a definition of unfold:
-- | unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
-- | unfold endF mapF f' start = map mapF (unfoldRec endF f' [start])
-- |         where unfoldRec endF f' (x:xs)  | endF (f' x) = (f' x:xs)
-- |                                         | otherwise = unfoldRec endF f' (f' x:x:xs)

-- | After looking at the lecture notes again, noticed I missed a map:
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold null head tail = map head . takeWhile (not.null) . iterate tail
-- | noting that: iterate f x = x : iterate f (f x)

-- | splits' :: [a] -> [(a,[a])]
-- | splits' xs = unfold (from one element to the next) (this is the last element) (first element)
splits' xs = unfold (\ (xs, _) -> null xs) 
                    (\ ((x:xs), ys) -> (x, xs++ys)) 
                    (\ (x:xs, ys) -> (xs, x:ys))
                    $ (xs,[])

-- | First shot at implementing permutations, this double counts some permutations:

-- | permutations :: [a] -> [[a]]
-- | permutations = map (uncurry insertAtAll) . splits

-- | insertAtAll x xs = [insertAt i x xs| i <- [0..length xs]]
-- | 
-- | insertAt 0 x [] = [x]
-- | insertAt i x (y:ys) | i == 0 = (x:y:ys)
-- |                     | otherwise = y:(insertAt (i-1) x ys)

-- | As it is in the sheet:
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [z:ys | (z, zs) <- splits xs, ys <- permutations zs]


-- | 6.5 

-- | reminder:
-- | foldr' :: (a -> b -> b) -> b -> [a] -> b
-- | foldr' f b [x] = f x b
-- | foldr' f b (x:xs) = f x (foldr' f b xs)

permutations' :: [a] -> [[a]]
permutations' = foldr (\ elem lists -> concat . map (include' elem) $ lists) [[]]

-- | *Main> permutations' [1,2,3]
-- | [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-- | *Main> all (`elem` permutations [1,2,3,4,5]) (permutations' [1,2,3,4,5])
-- | True
-- | *Main> all (`elem` permutations' [1,2,3,4,5]) (permutations [1,2,3,4,5])
-- | True

-- | First attempt (almost correct):
-- | include' x = foldr (\ elem (zs:zss) -> map (elem:) $ (zs:(x:zs):zss)) [[]]
include' :: a -> [a] -> [[a]]
include' x = foldr (\ elem ((z:zs):zss) -> (z:elem:zs) : (map (elem:) ((z:zs):zss))) [[x]]

-- | *Main> include' 99 []
-- | [[99]]
-- | *Main> include' 99 [1]
-- | [[99,1],[1,99]]
-- | *Main> include' 99 [1,2]
-- | [[99,1,2],[1,99,2],[1,2,99]]
-- | *Main> include' 99 [1,2,3]
-- | [[99,1,2,3],[1,99,2,3],[1,2,99,3],[1,2,3,99]]

-- | 6.6 
-- reminder:
-- unfold :: (a->Bool) -> (a->b) -> (a->a) -> a -> [b]
-- unfold = map head . takeWhile (not.null) . iterate tail

unfold' :: (a -> Maybe (b, a)) -> a -> [b]
unfold' nht = map fst . catMaybes . takeWhile (isJust) . iterateMaybe nht . nht

-- | noting that: iterate f x = x : iterate f (f x)
iterateMaybe f Nothing = Nothing : iterateMaybe f Nothing
iterateMaybe f (Just (x, y)) = Just (x, y) : iterateMaybe f (f y)

-- | selection sort with conventional unfold:

selectionSort xs = unfold null minimum excludeMin $ xs

excludeMin xs = excludeFirst (minimum xs) xs

excludeFirst x [] = []
excludeFirst x (y:ys) | y == x = ys
                 | otherwise = y: excludeFirst x ys

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

-- | unfold' :: ([a] -> Maybe (a, [a])) -> [a] -> [a]
selectionSort' xs = unfold' (\ xs -> case xs of [] -> Nothing
                                                xs -> Just (splitMin xs)) xs
       
splitMin xs = (minimum xs, excludeFirst (minimum xs) xs)
                where mini = minimum xs