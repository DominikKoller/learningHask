-- | Q 11.1

foldBool :: a -> a -> Bool -> a
foldBool _ false False = false
foldBool true _ True = true

data Day = Sunday | Monday   | Tuesday | Wednesday | 
                    Thursday | Friday  | Saturday

foldDay :: a->a->a->a->a->a->a->Day->a
foldDay sunday _ _ _ _ _ _    Sunday = sunday
foldDay _ monday _ _ _ _ _    Monday = monday
foldDay _ _ tuesday _ _ _ _   Tuesday = tuesday
foldDay _ _ _ wednesday _ _ _ Wednesday = wednesday
foldDay _ _ _ _ thursday _ _  Thursday = thursday
foldDay _ _ _ _ _ friday _    Friday = friday
foldDay _ _ _ _ _ _ saturday  Saturday = saturday

-- | Q 11.2
-- | <= on Bool corresponds to (renamed to <== to compare to <=):

True <== False = False
_ <== _ = True

-- *Main> True <= False
-- False
-- *Main> True <== False
-- False

data Set a = Empty | Singleton a | Union (Set a) (Set a)

foldSet :: b -> (a -> b) -> (b -> b -> b) -> Set a -> b
foldSet empty singleton union Empty = empty
foldSet empty singleton union (Singleton a) = singleton a
foldSet empty singleton union (Union a b) = union (foldSet empty singleton union a) 
                                                  (foldSet empty singleton union b)

isIn :: Eq a => a -> Set a -> Bool
isIn x = foldSet False (==x) (||)

subset :: Eq a => Set a -> Set a -> Bool
a `subset` b = foldSet True (\ x -> x `isIn` b) (&&) a

instance Eq a => Eq (Set a) where
    xs == ys = (xs `subset` ys) && (ys `subset` xs)

-- Q 12.1

type Queue a = [a]

empty :: Queue a
empty = []
-- This is cheap: one step

isEmpty :: Queue a -> Bool
isEmpty = null
-- This is cheap: one step

add :: a -> Queue a -> Queue a
add q qs = qs++[q]
-- This is expensive: 
-- n steps for length n of qs

pop :: Queue a -> (a, Queue a)
pop (q:qs) = (q,qs)
-- This is cheap:
-- one step

-- if the queue were represented by a list of its elements in the order in which they will leave the queue,
-- add would be cheap (one step) and pop would be expensive (n steps for length n of the queue)

type Queue' a = ([a], [a])

empty' :: Queue' a
empty' = ([],[])

isEmpty' :: Queue' a -> Bool
isEmpty' ([],[]) = True
isEmpty' _ = False

add' ::  a -> Queue' a -> Queue' a
add' q ([], back) = ([q], back)
add' q (front, back) = (front, q:back)

pop' :: Queue' a -> (a, Queue' a)
pop' ((f:fs), bs) = (f, (fs,bs))
pop' ([], back) = (last, (reverseInit, []))
    where (reverseInit, last) = reverseInitLast back

-- a tuple of the reverse init and the last element
reverseInitLast xs = rec [] xs
    where   rec revInit [x] = (revInit, x)
            rec revInit (x:xs) = rec (x:revInit) xs

-- performance of this queue:
-- Note first how adding and popping alternately are very cheap
-- Also add is always cheap
-- Pop is expensive in only one occasion: when there are a lot of elements in the back.
-- This is because the back has to be reversed for pop. However, the queue we get from pop
-- has all the elements in the front: popping those will be cheap again.
-- 
-- This queue is either faster or equally as fast as the more primitive queue.
-- Drawback: it is slightly less predictable in performance.


-- Q 12.2

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- *Main> fib 10
-- 55
-- (0.00 secs, 185,432 bytes)
-- *Main> fib 20
-- 6765
-- (0.04 secs, 7,617,536 bytes)
-- *Main> fib 30
-- 832040
-- (3.95 secs, 921,647,000 bytes)

-- fib 30 is very slow: it calculates a lot of fib values mutliple times.
-- we can say at least that it calculates more than twice as much as fib 28, and less than twice as much as fib 29
-- TODO find better estimate

-- two n = (fib n, fib (n+1))
-- two n = (fib n, fib n + fib (n-1))

two 0 = (0, 1)
two n = (fn, fn + fprev)
    where (fprev, fn) = two (n-1)

fib' n = fst (two n)

-- *Main> fib 30
-- 832040
-- (4.06 secs, 921,644,840 bytes)
-- *Main> fib' 30
-- 832040
-- (0.00 secs, 141,976 bytes)

-- fib' n takes n steps, hence approximately n*k seconds for some k

roughly :: Integer -> String
roughly n = head xs : "e" ++ show (length xs-1)
    where xs = show n

-- *Main> roughly (fib' 10000)
-- "3e2089"
-- (0.02 secs, 9,860,312 bytes)
-- Where fib 0 is the 'zeroth' number

-- from Sheet 4
type Matrix a = [[a]]

-- from lecture12.lhs
power :: Integral n => (a -> a -> a) -> a -> a -> n -> a
power (*) id x n -- x^n*id
    | n == 0 = id
    | even n = ((power (*) id) $! (x*x)) (n`div`2)
    | odd n  = ((power (*)) $! (x*id)) x (n-1)

mul :: Num a => Matrix a -> Matrix a -> Matrix a
mul a b = map (\ rowA -> map (dot rowA) (colsB)) a
    where colsB = cols b

cols :: [[a]] -> [[a]]
cols = foldr (\ xs acc -> zipWith (:) xs acc) (repeat [])

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

-- The following is specific to interger matrices of size 2x2
-- bad practice in general, sufficient for the scope of this question
pow :: Integral n => Matrix Integer -> n -> Matrix Integer
pow = power mul identity

identity = [[1,0],[0,1]]

f = [[0,1],[1,1]]

fib'' n = (head . tail . head) (pow [[0,1],[1,1]] n)