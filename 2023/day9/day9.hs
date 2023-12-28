main :: IO ()
main = do
    input <- readFile "aoc-23-9.txt"
    let input' = lines input
    let result1 = sum (map predict (map (\x -> map (\y -> read y :: Integer) x) (map words input')))
    let result2 = sum (map extrapolate (map (\x -> map (\y -> read y :: Integer) x) (map words input')))
    print result1
    print result2

zeroDiff :: [Integer] -> Bool
zeroDiff [] = True
zeroDiff [_] = True
zeroDiff (x : y : xs)
    | x == y = zeroDiff (y : xs)
    | otherwise = False

subtractPred :: [Integer] -> [Integer] -> [Integer]
subtractPred [] xs = reverse xs
subtractPred [_] xs = reverse xs 
subtractPred (y1 : y2 : ys) xs = subtractPred (y2 : ys) ((y2 - y1) : xs)

predict :: [Integer] -> Integer
predict [] = 0
predict (x : xs)
    | zeroDiff (x : xs) = last xs
    | otherwise = predict (subtractPred (x : xs) []) + last xs

extrapolate :: [Integer] -> Integer
extrapolate [] = 0
extrapolate (x : xs)
    | zeroDiff (x : xs) = x
    | otherwise = x - (extrapolate (subtractPred (x : xs) []))
