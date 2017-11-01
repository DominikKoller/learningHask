-- | Q 5.1 The predefined functions
-- | take :: Int -> [a] -> [a]
-- | drop :: Int -> [a] -> [a]
-- | divide a list into an initial segment and the rest, so that take n xs ++
-- | drop n xs = xs and take n xs is of length n or length xs, whichever is less.

-- | Write your own definitions for these functions and check that they give
-- | the same answer as the predefined functions for some representative
-- | arguments. Is take n xs strict in n? Is it strict in xs? Can it be strict
-- | in neither?

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

-- | 5.3 Define a function evens :: [a] -> [a] which returns a list of the elements
-- | of its input that are in even numbered locations:
-- | *Main> evens [’a’..’z’]
-- | "acegikmoqsuwy"
-- | and a function odds of the same type which returns the remaining elements.
-- | (Hint: you might use the one function in defining the other. . . )
-- | Suppose you need both evens xs and odds xs for the same xs. Find an
-- | alternative definition for
-- | > alternates :: [a] -> ([a],[a])
-- | > alternates xs = (evens xs, odds xs)
-- | which calculates the result in a single pass along the list.
-- | Ideally, you should derive the definition showing that it is right.

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
-- | alternates (x:y:xs) = (evens [x:y:xs], odds [x:y:xs])
-- |                     = (x:evens xs, y:odds xs)
-- |                     = (x:a, y:b) where (a, b) = (evens xs, odds xs)
-- |                     = (x:a, y:b) where (a, b) = alternates xs


-- | 6.1 Generalising an earlier exercise: 
-- | for finite types a, b and c there are as many functions of type a -> b -> c as there are are 
-- | of type (a, b) -> c (because (c^b)^a = c^ba ). 
-- | This correspondence, and a similar correspondence for infinte types, 
-- | is demonstrated by the (predefined) functions 
-- | 
-- | curry :: ((a,b) -> c) -> (a -> b -> c) 
-- | uncurry :: (a -> b -> c) -> ((a,b) -> c)
-- | 
-- | for which both curry.uncurry and uncurry.curry are identity functions (of the appropriate type). 
-- | These functions are uniquely determined by their types. 
-- | Write out what must be the definitions of these two functions, 
-- | and prove that they are mutually inverse. (If you type these definitions at an interpreter, 
-- | remember to change their names to avoid clashing with the Prelude functions.) 
-- | Does applying uncurry have any effect on strictness?

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
-- | g is the inverse of f iff f.g is an identity function on the type of f.

-- | 6.2 In the lecture, zip was defined by two equations whose left hand side
-- | patterns overlapped. The order of these two equations matters: what
-- | happens if they are switched? Find a set of defining equations whose
-- | order does not matter.

zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y):zip' xs ys
zip' [] _ = []
zip' _ [] = []

-- | *Main> zip [1..3] ['a'..'z']
-- | [(1,'a'),(2,'b'),(3,'c')]
-- | *Main> zip' [1..3] ['a'..'z']
-- | [(1,'a'),(2,'b'),(3,'c')]

-- | 6.3 The predefined function
-- | zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- | clearly, from its type, is related to zip. Give a definition of zipWith in
-- | terms of zip and other standard functions.
-- | In practice, zipWith is defined directly and zip is then defined in terms
-- | of zipWith. Write a recursive definition of zipWith and use it to define zip.

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

-- | 6.4 Write (perhaps using unfold to do so) a function
-- | > splits :: [a] -> [(a,[a])]
-- | which given a list xs returns a list of all the (x; as ++ bs) that satisfy
-- | as ++ [x] ++ bs = xs so that you can define for non-null xs
-- | > permutations xs =
-- | > [ x:zs | (x,ys) <- splits xs, zs <- permutations ys ]

splits :: [a] -> [(a,[a])]
splits [] = []
splits (x:xs) = (x,xs) : (map (\ (k, ys) -> (k, x:ys)) . splits $ xs)

-- | using unfold:

-- | Trying to come up with a definition of unfold:
-- | unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
-- | unfold endF mapF f' start = map mapF (unfoldRec endF f' [start])
-- |         where unfoldRec endF f' (x:xs)  | endF (f' x) = (f' x:xs)
-- |                                         | otherwise = unfoldRec endF f' (f' x:x:xs)

-- | After looking at the lecture notes again:
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold null head tail = map head . takeWhile (not.null) . iterate tail
-- | noting that: iterate f x = x : iterate f (f x)

-- | splits' :: [a] -> [(a,[a])]
-- | splits' xs = unfold (from one element to the next) (this is the last element) (first element)
splits' xs = unfold (\ (xs, _) -> null xs) 
                    (\ ((x:xs), ys) -> (x, xs++ys)) 
                    (\ (x:xs, ys) -> (xs, x:ys))
                    $ (xs,[])