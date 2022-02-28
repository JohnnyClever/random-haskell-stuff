fizzbuzz :: [Int] -> [Char]
fizzbuzz [] = []
fizzbuzz (x:xs)
    |x `mod` 15 == 0 = " fizzbuzz" ++ fizzbuzz xs 
    |x `mod` 3  == 0 = " buzz" ++ fizzbuzz xs
    |x `mod` 5  == 0 = " fizz" ++ fizzbuzz xs
    |otherwise       = " " ++ (show x) ++ fizzbuzz xs

fizzBuzz :: [Int] -> [Char]
fizzBuzz [] = []
fizzBuzz (x:xs) = (isFizz x) ++ (isBuzz x) ++ (isNormal x) ++ " " ++ (fizzBuzz xs)
    where
        isFizz x
            |x `mod` 5 == 0 = "fizz"
            |otherwise = []
        isBuzz x
            |x `mod` 3  == 0 = "buzz"
            |otherwise = []
        isNormal x
            |(x `mod` 5 /= 0) && (x `mod` 3 /= 0) = show x
            |otherwise = ""

