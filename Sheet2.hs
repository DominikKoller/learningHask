import Data.Char (toUpper)

-- | Q 3.1

-- | Wouldn't it make sense to have'x <= y = x == y || x < y' in the typeclass?
-- | Then I assume one could choose which of them to implement in an instance, right?

-- | I will make the length of a list the deciding factor for Ord' [a]
-- | I called the typeclass Ord' and added $$ to the operators to avoid conflict with the predefined ones
class Eq a => Ord' a where
    (<$$), (<=$$), (>$$), (>=$$) :: a -> a -> Bool
    x <$$ y = not (x >=$$ y)
    x >$$ y = not (x <=$$ y)
    x >=$$ y = x == y || x >$$ y

instance Eq a => Ord' [a] where
    [x] <=$$ [y] = length [x] < length [y]


-- | Q2, If h x y = f (g x y)

-- | i)
-- | Then h x y = (f.g) x y
-- | By definition of function composition
-- | Therefore h = f.g

-- | ii)
-- | Then by i) h x = (f.g) x
-- | But NOT h x = f.g x
-- | Since function application has precedence over function composition

-- | iii)
-- | by i) h x y = (f.g) x y

-- | Q 3.3
-- | 
-- | subst f g x = (f x) (g x) :: (a -> b -> c) -> (a -> b) -> a -> c
-- | fix f = f (fix f) :: (t -> t) -> t
-- | twice f = f . f :: (a -> a) -> a -> a
-- | selfie f = f f :: NOT possible, infinite type

-- | Q 3.4
-- | 
-- | [] : xs = xs
-- | is false: it adds [] as the first element of the list xs

-- | xs : [] = xs
-- | is true: it adds xs as the first (and only) element to [], giving [xs]

-- | [[]] ++ xs = xs 
-- | is false: [[]] ++ [[]] = [[],[]]
-- | and [[]] ++ [1] = TYPE ERROR since 1 isn't a list

-- | [[]] ++ [xs] = [[], xs]
-- | is true, the first list has [] as its only element, in the second list xs is the element
-- | only as long as xs is a list though

-- | [] : xs = [[], xs]
-- | is false: [] : xs is [[], x0, x1, ...], where x0 is the first element of xs, x1 the second, ...
-- | also these elements have to be lists

-- | xs : xs = [xs, xs]
-- | this is false: xs : xs will give us [xs, x0, x1, ...] with x0, x1, ... as above
-- | and only work if xs is a list

-- | [[]] ++ xs = [xs]
-- | this is false: it will give us [[], x0, x1, ...] with x0, x1, ... as above
-- | also xs needs to be a list of list

-- | [xs] ++ [] = [xs]
-- | This is true, [] is the identity element of (++)

-- | xs : [] = xs
-- | this is false, def of (:) says it will be [xs]

-- | xs : [xs] = [xs, xs]
-- | this is true, : adds element xs as first element to [xs] 

-- | [[]] ++ xs = [[], xs]
-- | This is true, but only works if xs is a list as the type of [[]] is list of list

-- | [xs] ++ [xs] = [xs, xs]
-- | This is true, (++) makes a list from all the elements of the first and then the second list
-- | Its performance will depend on the length first list

-- | Q 4.1 
-- | if f and g are strict, and f . g is not a type error, then
-- | g :: a -> b
-- | f :: b -> c
-- | for some non-generic types a, b and c
-- | then f.g :: a -> c for the non-generic types a and c

-- | if in the above example we only know that f.g is strict,
-- | it might be that b is a generic type, so it is not guaranteed that
-- | either f or g are strict

-- | Q 4.2
-- | There are now 3^3 functions of Bool->Bool
-- | There is no way to destinguish between bottom and True or False in the inputs
-- | Hence the only such functions that are computable are the ones that
-- | output the same value for all inputs (ie that completely disregard the input). 
-- | Also, a function with output bottom is not computable. 
-- | Hence the only two computable functions are:
boolTFB _ = True
boolTFB' _ = False
-- | However, in Haskell the non-computable function
boolTFB'' _ = bottom
    where bottom = bottom
-- | is definable as well.

-- | Q 4.3
-- | False && undefined == False
-- | undefined && False == ERROR
-- | undefined && undefined == ERROR
-- | Meaning (&&) must be
-- | x (&&) y | not x = False
-- |          | not y = False
-- |          | otherwise = True

-- | Or with Pattern Matching
-- | True && y = y
-- | False && _ = False

-- | As when the first parameter is False, (&&) does not look at the second parameter
-- | But (&&) always looks at the first parameter

-- | The given function (&&&) will output False whenever at least one of the inputs is False. 
-- | If both its inputs are True, it will output True.
-- | Given that the datatype of both inputs and output is Bool, which has only the values
-- | True, False, and Bottom, all cases that remain unspecified are
-- | 1) bottom &&& bottom
-- | 2) True &&& bottom
-- | 3) bottom &&& True
-- | All three of these are necessarily uncomputable. 
-- | 1) can never check whether its inputs are bottom or will eventually compute.
-- | 2) has to check whether the second input is True or Bottom - if it is true, it has to 
-- | output True, if it is False, it has to output False. Hence the function is uncomputable.
-- | 3) is symmetrical to 2)

-- | Hence all cases not specified return bottom, and therefore there is only one such function.

-- | It is computable only if we check both input parameters in parallel - otherwise, 
-- | if the first one is bottom, the function will never 'reach' the second one or vice versa.

-- | I do not know any way in Haskell to define this function.
-- | My intuition was:

(&&&) :: Bool -> Bool -> Bool
False &&& _ = False
_ &&& False = False
True &&& True = False

-- | But this will fail when the first parameter is undefined, since it will check whether it is False
-- | We have to check both parameters in parallel, cancelling both computations only when one has returned false
-- | or both have returned true.

-- | Q 4.4

song :: Int -> String
song 1 = verse 1 ++ "\n"
song n = song (n-1) ++ "\n" ++ verse n ++ "\n"
verse n = line1 n ++ "\n" ++ line2 ++ "\n" ++ line3 n ++ "\n" ++ line2

line1 1 = "One man went to mow"
line1 n = capitalizeFirst (parseNum n) ++ " men went to mow"

line2 = "Went to mow a meadow"

line3 n = capitalizeFirst (listMen n) ++ " and his dog"

listMen 1 = "one man"
listMen n = parseNum n ++ " men, " ++ listMen (n-1)

capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = toUpper x : xs

-- | The rest is just converting numbers from 0-999999 to String
xNames :: [String]
xNames = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

xxNames :: [String]
xxNames = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

cNames :: [String]
cNames = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

quantize :: Int -> Int -> Int
quantize x q = q * (x `div` q)

parseNum :: Int -> String
parseNum n
    | n < 0 = "negative " ++ parseNum (-n)
    | n < 10 = xNames !! n
    | n < 20 = xxNames !! (n-10)
    | n < 100 && n `mod` 10 == 0 = cNames !! ((n `div` 10)-2)
    | n < 100 = parseNum (quantize n 10) ++ "-" ++ parseNum (n `mod` 10)
    | n < 1000 && n `mod` 100 == 0 = parseNum (n `div` 100) ++ " hundred"
    | n < 1000 = parseNum (quantize n 100) ++ " and " ++ parseNum (n `mod` 100) 
    | n < 1000000 = parseNum (n `div` 1000) ++ " thousand " ++ parseNum (n `mod` 1000)

