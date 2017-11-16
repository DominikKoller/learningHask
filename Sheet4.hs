-- Sheet 4

-- | Q 7.1

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- cp xss]

cp' :: [[a]] -> [[a]]
cp' = foldr (\newxs acc  -> concat' . map' (\xs -> map' (:xs) newxs) $ acc) [[]] 

concat' :: [[a]] -> [a]
concat' = foldr (++) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ x acc -> f x:acc) []

-- Q 7.2

cols :: [[a]] -> [[a]]
cols [xs] = [ [x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

cols' :: [[a]] -> [[a]]
cols' = foldr (\ xs acc -> zipWith (:) xs acc) (repeat [])
-- Without fold:
-- cols' [] = repeat []
-- cols' (xs:xss) = zipWith (:) xs (cols' xss)


-- This gives us an infinite list for [] though.
-- The following avoids that, but also has different behavior 
-- when there are lists with different sizes
cols'' :: [[a]] -> [[a]]
cols'' = foldr (\ xs acc -> zipWith (:) xs (cycle acc)) [[]]
-- Without fold:
--cols'' [] = [[]]
--cols'' (xs:xss) = zipWith (:) xs (cycle (cols'' xss))

-- | I was first thinking along the lines of:
-- | cols' xss = map head xss : cols' (map tail xss)
-- | But this does gives empty list exceptions (which could prob be filtered)
-- | And is not a fold

-- 7.3

data Pist a = Wrap a | Pons a (Pist a) deriving Show

-- reminder:
-- fold b _ [] = b
-- fold b f [x] = f x b -- as there is no empty Pist, we need this analogy
-- fold b f (x:xs) = f x (fold b f xs)

foldp :: (a -> b) -> (a -> b -> b) -> Pist a -> b

foldp wrap cons (Wrap x) = wrap x
foldp wrap cons (Pons a as) = cons a (foldp wrap cons as)

-- foldp Wrap Pons
-- is the identity. Prove by induction:
-- to prove: foldp Wrap Pons xs = xs
-- for xs :: Pist a
-- Base case: xs = Wrap x
-- foldp Wrap Pons (Wrap x) = Wrap x (by definition of foldp)
-- Inductive step. IH: foldp Wrap Pons xs = xs
-- To prove: foldp Wrap Pons (Pons x xs) = Pons x xs
-- foldp Wrap Pons (Pons x xs)
-- Pons x (foldp Wrap Pons xs)
-- {IH}
-- Pons x xs
-- Therefore foldp Wrap Pons xs = xs
-- for xs :: Pist a

pist :: [a] -> Pist a
pist [x] = Wrap x
pist (x:xs) = Pons x (pist xs)

list :: Pist a -> [a]
list (Wrap x) = [x]
list (Pons x xs) = x:list xs

-- pist.list is the identity function on the type List. Prove by induction:
-- to prove: (pist.list) xs = xs
-- Base case: xs = Wrap x
-- This would be nicer if we had an empty Pist.
-- (pist.list) (Wrap x)
-- (pist (list (Wrap x)))
-- (pist [x])
-- Wrap x

-- Inductive Step. IH: (pist.list) xs = xs. To show: (pist.list) (Pons x xs) = Pons x xs
-- (pist.list) (Pons x xs)
-- (pist (list (Pons x xs)))
-- (pist (x:list xs))
-- Pons x (pist (list xs))
-- Pons x ((pist.list) xs)
-- Pons x xs (by IH)

-- Therefore (pist.list) xs = xs
-- prove for (list.pist) goes analogously, (list.pist) is the identity function on the type of Pist


-- fold1 wrap cons = foldp wrap cons . pist
-- fold1 wrap cons looks like a fold for non-empty lists. This would look like:
-- fold1 wrap cons [x] = wrap x
-- fold1 wrap cons (x:xs) = cons x (fold1 wrap cons xs)

-- To prove this:
-- fold1 wrap cons [x]
-- (foldp wrap cons . pist) [x]
-- foldp wrap cons (pist [x])
-- foldp wrap cons (Wrap x)
-- {def of foldp}
-- wrap x
-- Therefore
-- fold1 wrap cons [x] = wrap x
-- 
-- fold1 wrap cons (x:xs)
-- (foldp wrap cons . pist) (x:xs)
-- foldp wrap cons (pist (x:xs))
-- foldp wrap cons (Pons x (pist xs))
-- cons x (foldp wrap cons (pist xs))
-- cons x ((foldp wrap cons . pist) xs)
-- {def of fold1}
-- cons x (fold1 wrap cons xs)
-- Therefore 
-- fold1 wrap cons (x:xs) = cons x (fold1 wrap cons xs)

-- Therefore
fold1 wrap cons [x] = wrap x
fold1 wrap cons (x:xs) = cons x (fold1 wrap cons xs)

-- Writing cols as an instance of fold1:
cols''' :: [[a]] -> [[a]]
cols''' = fold1 (\ xs -> [ [x] | x <- xs])
                (\ xs acc -> zipWith (:) xs acc)

-- foldr1 :: (a -> a -> a) -> [a] -> a
-- foldr1 f [x] = x
-- foldr1 f (x:xs) = f x (foldr1'' f xs) 
--                           This ^
-- was wrong in the problem sheet! Forgot the f

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' = fold1 id

-- you cannot write fold1 warp cons as an instance of foldr1.
-- Look at the types: fold1 has less restrictions on its types

-- Q 8.1

ljustify :: Int -> String -> String

ljustify 0 _ = ""
ljustify n [] = replicate n ' '
ljustify n (s:ss) = s: ljustify (n-1) ss

-- ljustify 10 "word"
-- "word      "

-- ljustify 5 "RATATAT"
-- "RATAT"

rjustify :: Int -> String -> String
rjustify n s = replicate (n-l) ' ' ++ drop (l-n) s
    where l = length s

-- rjustify 10 "word"
-- "      word"

-- rjustify 4 "Ratatat"
-- "atat"

-- the functions cut the words on the left or right hand side
-- whenever the word is too long.

-- alternative behaviour would be to always return at least the word
-- or to return an array of strings, representing lines, or inserting newline characters

-- Q 8.2

type Matrix a = [[a]]

scale :: Num a => a -> Matrix a -> Matrix a
scale s= map (map (*s))

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

add :: Num a => Matrix a -> Matrix a -> Matrix a
add mx my= zipWith (zipWith (+)) mx my

mul :: Num a => Matrix a -> Matrix a -> Matrix a
mul a b= zipWith (zipWith (*)) a b

-- Matrix product. Dot products of every row in A with every column in B
-- not asked in the sheet
prod :: Num a => Matrix a -> Matrix a -> Matrix a
prod a b = map (\ rowA -> map (dot rowA) (colsB)) a
    where colsB = cols b

table :: Show a => Matrix a -> String
table m = unlines . map (unwords.(map show)) $ m

-- *Main> putStr (table [[2,3],[1,2],[3,4]])
-- 2 3
-- 1 2
-- 3 4
-- 
-- *Main> putStr (table [[2,3,4],[1,2,3]])
-- 2 3 4
-- 1 2 3