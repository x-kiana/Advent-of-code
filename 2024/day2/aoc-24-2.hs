main :: IO ()
main = do
    input <- readFile "input_2.txt"
    let input' = lines input
    let result1 = sum (map safe (map (\x -> map (\y -> read y :: Integer) x) (map words input')))
    let result2 = sum (map safeDamp (map (\x -> map (\y -> read y :: Integer) x) (map words input')))
    print result1
    print result2

graduallyIncreasing :: [Integer] -> Bool
graduallyIncreasing [] = True
graduallyIncreasing [_] = True
graduallyIncreasing (x : y : xs)
    | and [(x < y), (y - x <= 3)] = graduallyIncreasing (y : xs)
    | otherwise = False 

graduallyDecreasing :: [Integer] -> Bool 
graduallyDecreasing [] = True
graduallyDecreasing [_] = True
graduallyDecreasing (x : y : xs)
    | and [(x > y), (x - y <= 3)] = graduallyDecreasing (y : xs)
    | otherwise = False 

safe :: [Integer] -> Integer
safe [] = 1
safe xs 
    | graduallyIncreasing xs = 1
    | graduallyDecreasing xs = 1
    | otherwise = 0

oneLevelRemoved :: [a] -> [[a]]
oneLevelRemoved [] = [[]]
oneLevelRemoved [_] = [[]]
oneLevelRemoved (x : xs) = xs : (map (\y -> x : y) (oneLevelRemoved xs))

safeDamp :: [Integer] -> Integer
safeDamp [] = 1
safeDamp xs
    | any graduallyIncreasing (oneLevelRemoved xs) = 1
    | any graduallyDecreasing (oneLevelRemoved xs) = 1
    | otherwise = 0
