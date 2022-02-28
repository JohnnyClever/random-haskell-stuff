import Data.List

main = putStrLn "Hello World"

removeDivByX :: Int -> [Int] -> [Int]
removeDivByX _ [] = []
removeDivByX x (y:ys) 
    | mod y x == 0  = removeDivByX x ys
    | otherwise = y : removeDivByX x ys

keepitemDivByX :: Int -> [Int] -> [Int]
keepitemDivByX _ [] = []
keepitemDivByX x (y:ys) 
    | mod y x == 0  = y : keepitemDivByX x ys
    | otherwise = keepitemDivByX x ys

intersection :: Eq a => [a] -> [a] -> [a]
intersection [] _ = []
intersection (x:xs) l 
    | elem x l = x : intersection xs l
    | otherwise = intersection xs l


euler1 x y z = sum ((keepitemDivByX x [1..z])++(keepitemDivByX y [1..z])) - sum(intersection (keepitemDivByX x [1..z]) (keepitemDivByX y [1..z]))


listhalve :: [a] -> ([a],[a])
listhalve xs = listhalve' [] xs
    where
listhalve' :: [a] -> [a] -> ([a],[a])
listhalve' x y
    | length x == length y = (x, y)
    | length x <  length y = listhalve' (x ++ ([head y])) (tail y)
    | length x >  length y = (x, y)

sumsquares :: [Int] -> Int
sumsquares xs = sum [x^2 | x <- xs]

myReplicate :: Int -> a -> [a]
myReplicate 0 x = []
myReplicate n x = map (fst) [ (x,y) | y <- [1..n] ]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

symEq :: Eq a => (a,a,a) -> (a,a,a) -> Bool
symEq (x,y,z) (u,v,w) = (x == u && y == v) || (x == v && y == u)

removeDuplTuples :: Eq a => [(a,a,a)] -> [(a,a,a)]
removeDuplTuples = nubBy symEq

pythagtriple :: Int -> [(Int,Int,Int)]
pythagtriple 0 = []
pythagtriple n = removeDuplTuples (keepPythag [(x,y,z)| x <- [1..n], y <- [1..n], z <- [1..n]])
    where 
        keepPythag :: [(Int,Int,Int)] -> [(Int,Int,Int)]
        keepPythag ((x,y,z):xs)
            |x*x + y*y == z*z = [(x,y,z)] ++ keepPythag xs
            |otherwise =  keepPythag xs
keepPythag []=[]

myexp :: Integer -> Integer -> Integer
myexp 0 0 = error "0^0 is undefined!"
myexp _ 0 = 1
myexp x y = x*(myexp x (y-1))

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
    |x<=y = x:(merge xs (y:ys))
    |x>y = y:(merge (x:xs) ys)
    |otherwise = error "Error: sets may not have been ordered"
merge xs [] = xs
merge [] xs = xs


mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort xs
    |isSorted xs = xs
    |otherwise = mergesort' (listhalve xs)
    where 
        mergesort' :: Ord a => ([a],[a]) -> [a]
        mergesort' (xs, ys) = merge (mergesort (xs)) (mergesort (ys))

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (x:[]) = True
isSorted (x:xs)
    |x <= (head xs) = isSorted xs
    |x > (head xs) = False
    |otherwise = True

listAtN :: Int -> [a] -> a
listAtN 1 (x:xs) = x
listAtN n []     = error "n too large or list too small"
listAtN n (x:xs) = listAtN (n-1) xs 

euclidNorm :: [Float] -> Float
euclidNorm2 :: [Float] -> String
euclidNorm  x = sqrt(sum (zipWith (*) x x))
euclidNorm2 x = "sqrt(" ++ (show $ sum (zipWith (*) x x)) ++ ")"++ " = " ++ show (sqrt (sum ( zipWith (*) x x)))

distance  x y = euclidNorm  (zipWith (-) x x)
distance2 x y = euclidNorm2 (zipWith (-) x x)

midpoint :: (Num a, Fractional a) => [a] -> [a] -> [a]
midpoint x y = zipWith ( / ) (zipWith (+) x y) ([2])


cartToPolar [x,y]
    | x > 0  = [r, atan(y/x) + 0]
    | x < 0  = [r, atan(y/x) +pi] 
    | x == 0 = [y, pi/2] where 
        r = euclidNorm [x,y]

cartToPolarDeg [x,y]
    | x > 0  = [r, (atan(y/x) + 0)*180/pi]
    | x < 0  = [r, (atan(y/x) +pi)*180/pi] 
    | x == 0 = [y, pi/2] where 
        r = euclidNorm [x,y]

cartToPolar2 [x,y]
    | x >  0 = ((     r),"atan(" ++ (show y) ++ "/" ++ (show x) ++ " )"  )
    | x <  0 = ((     r),"atan(" ++ (show y) ++ "/" ++ (show x) ++ " pi)")
    | x == 0 = ((show y), "pi/2") where
        r = "sqrt(" ++ (show (x*x + y*y)) ++ ")"

cMult :: Num a => (a,a) -> (a,a) -> (a,a)
cMult (x,y) (w,z) = (x*w-y*z,x*z+y*w)

cDiv :: Fractional a => (a, a) -> (a, a) -> (a, a)
cDiv (x,y) (w,z) = (x/(w*w+z*z),y/(w*w+z*z)) `cMult` (w,-z)

vAdd :: (Num c, Floating c) => [c] -> [c] -> [c]
vAdd x y = zipWith (+) x y

vSubtract :: (Num c, Floating c) => [c] -> [c] -> [c]
vSubtract x y = zipWith (-) x y

vMult x y = [x*z | z <- y]

polarToCart :: Floating a => [a] -> [a]
polarToCart [r,theta] = [r*cos(theta*pi/180), r*sin(theta*pi/180)]

vAddPolar [r1, theta1] [r2,theta2] = cartToPolarDeg (polarToCart [r1, theta1] `vAdd` polarToCart [r2,theta2])   


vSubtractPolar [r1, theta1] [r2,theta2] = cartToPolarDeg (polarToCart [r1, theta1] `vSubtract` polarToCart [r2,theta2])   

vDot x y = sum (zipWith (*) x y)

percentDiff :: Floating a => a -> a -> a
percentDiff x y = x-y

promys1 x y z = sum [x^3,y^3,z^3]

promys1Alt x y z = sum [x^2,y^2,z^2]

stringToNum :: [Char] -> [Char]
stringToNum x
    | x==[] = ""
    | [head x]== "_" = "0" ++ ", " ++ (stringToNum (tail x))
    | [head x]== " " = "0" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "a" = "1" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "b" = "2" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "c" = "3" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "d" = "4" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "e" = "5" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "f" = "6" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "g" = "7" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "h" = "8" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "i" = "9" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "j" = "10" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "k" = "11" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "l" = "12" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "m" = "13" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "n" = "14" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "o" = "15" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "p" = "16" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "q" = "17" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "r" = "18" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "s" = "19" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "t" = "20" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "u" = "21" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "v" = "22" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "w" = "23" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "x" = "24" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "y" = "25" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "z" = "26" ++ ", " ++ (stringToNum (tail x))
    | [head x]== "" = "" ++ (stringToNum (tail x))
    | otherwise = error "non-alphabetical input"
