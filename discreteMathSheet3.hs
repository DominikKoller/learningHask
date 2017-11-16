import Test.QuickCheck.Text

-- | 3.6

a :: Int -> Int
a 1 = 1
a n = n * a (n `div` 2)

aName :: Int -> String
aName 1 = show 1
aName n = "a_"++show n++" = " ++ show n ++ "*a_"++show next ++"\n"++ aName next
    where next = (n `div` 2)

bla :: Float -> Float
bla n = n**(logBase 2 n)
