countDigits :: Integer -> Integer
countDigits n = sum.digitList$n

digitList :: Integer -> [Integer]
digitList n | n < 10 = [n]
            | otherwise = digitList (n `div` 10) ++ [n `mod` 10]

lastQuestion n m = product [1..(m*n)] `mod` ((product [1..m])^n) == 0