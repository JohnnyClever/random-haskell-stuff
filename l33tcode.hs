--start of solution1, which picks out self-dividing numbers in a list

solution1 :: [Integer] -> [Integer]
solution1 []=[]
solution1 (x:xs)
    |check x = [x] ++ solution1 (xs)
    |otherwise = solution1 xs


digits :: Integer -> [Integer]
digits = map (read . (:[])) . show

check x 
    |elem 0 (digits x) = False
    |sum (map (x `mod`) (digits x)) == 0 = True
    |otherwise = False

-- the end of solution1

--start of solution2, which minimizes run-length encoded compressions of string by deleting characters

solution2      bvnnnnnnnnnnb