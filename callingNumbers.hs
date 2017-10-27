xNames :: [String]
xNames = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

xxNames :: [String]
xxNames = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

cNames :: [String]
cNames = map (++"ty") ["twen", "thir", "for", "fif", "six", "seven", "eigh", "nine"]

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