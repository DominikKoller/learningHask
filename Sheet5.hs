-- | Sheet 5
-- | Q 9.1

data Nat = Zero | Succ Nat

instance Show Nat where
    show n = "_" ++ show (int n) ++ "_"

int :: Nat -> Int
int Zero = 0
int (Succ n) = 1 + int n

-- | *Main> int (Succ (Succ Zero))
-- | 2

nat :: Int -> Nat
nat n | n==0 = Zero
      | n>0 = Succ (nat (n-1))
      | otherwise = undefined

-- | to emphasise the recursive datatype, these were made with "deriving Show" instead of the custom instance implementation
-- | *Main> nat 0
-- | Zero

-- | *Main> nat 9
-- | Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))

-- | *Main> nat (-1)
-- | *** Exception: Prelude.undefined

add :: Nat -> Nat -> Nat
add Zero n = n -- this is not needed but sometimes provides a shortcut (wont implement this for other operations)
add n Zero = n
add a (Succ b) = add (Succ a) b
-- | a + (b+1) = (a+1) + b

-- | *Main> add (Succ (Succ (Succ Zero))) (Succ Zero)
-- | _4_

mul :: Nat -> Nat -> Nat
mul n Zero = Zero
mul a (Succ b) = add a (mul a b)
-- | a * (b+1) = a  + a*b

-- | *Main> mul (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
-- | _6_

pow :: Nat -> Nat -> Nat
pow n Zero = Succ Zero
pow a (Succ b) = mul a (pow a b)
-- | a^(b+1) = a * a^b

tet :: Nat -> Nat -> Nat
tet n Zero = Succ Zero
tet a (Succ b) = pow a (tet a b)
-- | a 'tet' (b+1) = a^(a 'tet' b)

-- | thinking about the base case:
-- | we want:
-- | a 'tet' (1) = a
-- | and:
-- | a 'tet' (0+1) = a^(a 'tet' 0)
-- | and: a^1 = a
-- | therefore:
-- | a 'tet' 0 = 1

-- | *Main> (nat 2) `tet` (nat 2)
-- | _4_

-- | *Main> (nat 2) `tet` (nat 3)
-- | _16_

-- | *Main> (nat 2) `tet` (nat 4)
-- | _65536_
-- | (343.21 secs, 148,930,844,376 bytes)

-- | *Main> (nat 3) `tet` (nat 2)
-- | _27_

-- | Q 9.2

-- | The fold for Nat is characterized by replacing each of the constructors of Nat with the arguments of the fold function.
-- | eg foldNat zero succ (Succ (Succ Zero)) = (succ (succ (succ zero))

-- | Therefore:
foldNat zero succ Zero = zero
foldNat zero succ (Succ n) = succ (foldNat zero succ n)

-- | I take that 'destructor' means the same thing as 'selectors' in the notes.
-- | That is, inverting the constructor, returning one of its arguments

-- | Then, the destructor for Nat would be (avoiding nameclas with Prelude.pred)
predd (Succ n) = n

-- | There would not be a second one since Zero does not take any arguments

-- | reminder: the unfold on lists
-- | unfold :: (a->Bool) -> (a->b) -> (a->a) -> a -> [b]
-- | unfold null head tail =
-- | map head . takeWhile (not.null) . iterate tail


-- | informal description of what I first tried: 
-- | unfoldNat creates an infinite Nat, then steps through it and checks when to stop

-- | unfoldNat :: (Nat -> Bool) -> Nat
-- | unfoldNat stop = takeWhileNat (not.stop) infiniteNat
-- | 
-- | infiniteNat :: Nat
-- | infiniteNat = Succ infiniteNat
-- | 
-- | takeWhileNat :: (Nat -> Bool) -> Nat -> Nat
-- | takeWhileNat _         Zero = Zero
-- | takeWhileNat predicate s@(Succ p) | predicate s = Succ (takeWhileNat predicate p)
-- |                                   | otherwise = Zero

-- modeled after:
-- | takeWhile' :: (a -> Bool) -> [a] -> [a]
-- | takeWhile' predicate [] = []
-- | takeWhile' predicate (x:xs) | predicate x = x: takeWhile' predicate xs
-- |                             | otherwise = []

-- | However, this does not work since (of course) the predicate has to evaluate the full infinite Nat, and therefore will never finish.
-- | The difference to lists here is that we do not have an _element_ of the list on which we can base our predicate on.
-- | I now need to find an implementation of an unfold that is not so closely modeled after the unfold on lists.

unfoldNat :: (Nat -> Bool) -> Nat
unfoldNat p = buildNat p Zero
    where buildNat p n | not (p n) = n
                       | otherwise = buildNat p (Succ n)

-- | This implementation starts with Zero and unfolds until it reaches a state where the predicate function returns true
-- | I feel like this is not exactly unfold though, as unfold should look at what the constructor contains, not the constructor itself
-- | However, this is impossible with Nat because Nat contains nothing

-- | I cannot see how you could implement `int` with an unfold
-- | Does an 'unfold on Nat' not always return a Nat? Like the unfold on lists always returns a list? 
-- | I might invent some accumulator that one can choose to build up during the unfold,
-- | that is of an arbitrary type, that is used to determine the predicate condition.
-- | This would seem like a way too complicated method of creating something equivalent to unfold on lists.

int' :: Nat -> Int
int' = foldNat 0 (+1);

nat' :: Int -> Nat
nat' i = unfoldNat (\n -> int' n /= i)
-- | This is very inefficient since it needs to do int on every step

-- TODO if I have a lot of time: 
-- Come up with unfoldNat that passes a tuple of the Nat and an arbitrary accumulator

add' :: Nat -> Nat -> Nat
add' n = foldNat n Succ

-- *Main> add' Zero Zero
-- _0_
-- *Main> add' Zero (Succ Zero)
-- _1_
-- *Main> add' (Succ Zero) Zero
-- _1_
-- *Main> add' (Succ (Succ Zero)) (Succ Zero)
-- _3_

mul' :: Nat -> Nat -> Nat
mul' n = foldNat Zero (add' n)

-- *Main> mul' (Succ Zero) (Succ Zero)
-- _1_
-- *Main> mul' (Succ Zero) (Succ (Succ (Succ Zero)))
-- _3_
-- *Main> mul' (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))
-- _6_

pow' :: Nat -> Nat -> Nat
pow' n = foldNat (Succ Zero) (mul' n)

tet' :: Nat -> Nat -> Nat
tet' n = foldNat (Succ Zero) (pow' n)

-- | Q 10.1 on paper
-- | start of Q 10.2 on paper
-- | TODO Q 10.2 rest

-- | Q 10.4

data Liste a = Lin | Snoc (Liste a) a deriving Show

cat :: Liste a -> Liste a -> Liste a
cat xs Lin = xs
cat xs (Snoc ys y) = Snoc (cat xs ys) y

-- | *Main> cat (Snoc (Snoc Lin 0) 1) (Snoc (Snoc Lin 2) 3 )
-- | Snoc (Snoc (Snoc (Snoc Lin 0) 1) 2) 3

-- folde should replace all constructors of Liste with the arguments of folde
-- eg folde lin snoc (Snoc (Snoc Lin 0)1) = (snoc (snoc lin 0)1)
-- Therefore:

folde :: a -> (a -> b -> a) -> Liste b -> a
folde lin snoc Lin = lin
folde lin snoc (Snoc xs x) = snoc (folde lin snoc xs) x

-- *Main> folde [] (flip (:)) (Snoc (Snoc Lin 2)3)
-- [3,2]
-- *Main> folde [] (\acc e -> acc++[e]) (Snoc (Snoc Lin 2)3)
-- [2,3]-- 

list :: Liste a -> [a]
list = folde [] (\acc e -> acc++[e])

liste :: [a] -> Liste a
liste = foldr cons Lin

cons :: a -> Liste a -> Liste a
cons x xs = cat (Snoc Lin x) xs

-- *Main> liste [1,2,3,4]
-- Snoc (Snoc (Snoc (Snoc Lin 1) 2) 3) 4

-- *Main> list.liste $ [1,2,3,4,5]
-- [1,2,3,4,5]
-- *Main> list.liste $ []
-- []

-- | liste returns bottom when applied to an infinite list. TODO describe why?
-- | The infinite objects of type Liste a:

takeOuter :: Int -> Liste a -> Liste a
takeOuter _ Lin = Lin
takeOuter 0 xs = Lin
takeOuter n (Snoc xs x) = Snoc (takeOuter (n-1) xs) x

-- *Main> takeOuter 3 $ Snoc (Snoc (Snoc (Snoc (Snoc Lin 0)1)2)3)4
-- Snoc (Snoc (Snoc Lin 2) 3) 4

repeatListe :: a -> Liste a
repeatListe x = Snoc (repeatListe x) x

-- repeatListe gives an infinite object of type Liste. It starts with the last element (of the corresponding list)

revFolde :: a -> (b -> a -> a) -> Liste b -> a
revFolde lin snoc Lin = lin
revFolde lin snoc (Snoc xs x) = revFolde (snoc x lin) snoc xs

-- revFolde can (as expected) not work on infinite lists:
-- *Main> takeOuter 5 ( folde Lin Snoc (repeatListe 5) )
-- Snoc (Snoc (Snoc (Snoc (Snoc Lin 5) 5) 5) 5) 5

-- *Main> revFolde Lin (flip Snoc) (repeatListe 5)
-- Interrupted.

-- list :: Liste a -> [a]
-- list = folde [] (\acc e -> acc++[e])
-- 
-- liste :: [a] -> Liste a
-- liste = foldr cons Lin
-- 
-- cons :: a -> Liste a -> Liste a
-- cons x xs = cat (Snoc Lin x) xs


list' :: Liste a -> [a]
list' = revFolde [] (:)

-- *Main> list' (Snoc (Snoc (Snoc (Snoc Lin 1) 2) 3) 4)
-- [1,2,3,4]

liste' :: [a] -> Liste a
liste' = foldl Snoc Lin

-- *Main> liste' [1,2,3,4]
-- Snoc (Snoc (Snoc (Snoc Lin 1) 2) 3) 4

-- Q 10.5

-- | reminder: the unfold on lists
-- | unfold :: (a->Bool) -> (a->b) -> (a->a) -> a -> [b]
-- | unfold null head tail =
-- | map head . takeWhile (not.null) . iterate tail

unfolde :: (a -> Bool) -> (a->b) (a->a) -> a -> Liste b
unfolde stop mape next x | stop x = Lin
                            | otherwise = Snoc (unfolde stop mape next (next x)) (mape x) 

mape :: (a -> b) -> Liste a -> Liste b
mape _ Lin = Lin
mape f (Snoc xs x) = Snoc (mape f xs) (f x)

iteratee :: (a->a) -> a -> Liste a
iteratee f x = Snoc (iteratee f x) (f x)

takeWhilee :: (a -> Bool) -> Liste a -> Liste a
takeWhilee _ Lin = Lin
takeWhilee p (Snoc xs x) | p x = Snoc (takeWhilee p xs) x
                         | otherwise = Lin