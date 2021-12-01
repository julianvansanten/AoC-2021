module Main where
import System.Environment ( getArgs )

-- Exercise 1
-- Count the number of numbers that are larger than the previous in an array.
countIncreasing :: [Int] -> Int
countIncreasing list = countTrueValues $ getIncreasingArray list []

-- Create an array of booleans which tell whether the next element is bigger than the previous.
getIncreasingArray :: [Int] -> [Bool] -> [Bool]
getIncreasingArray [x,y] bools = bools ++ [y > x]
getIncreasingArray (x:y:xs) [] | y > x = getIncreasingArray (y:xs) [False, True]
                        | otherwise = getIncreasingArray (y:xs) [False, False]
getIncreasingArray (x:y:xs) bools | y > x = getIncreasingArray (y:xs) (bools ++ [True])
                            | otherwise = getIncreasingArray (y:xs) (bools ++ [False])
getIncreasingArray _ bools = bools

-- Count the amount of occurrences of True in a given array.
countTrueValues :: [Bool] -> Int
countTrueValues = length . filter (True ==)


--Exercise 2
-- Convert a given array to a sliding window of size 3.
getTotalWindow :: [Int] -> [Int]
getTotalWindow list = getWindow list []

-- Get the array of windows which contain the sum of some three values.
getWindow :: [Int] -> [Int] -> [Int]
getWindow [x,y,z] windows = windows ++ [x+y+z]
getWindow (x:y:z:xs) windows = windows ++ [x+y+z] ++ getWindow (y:z:xs) windows
getWindow _ windows = windows


-- Read a file with a given name
main :: IO ()
main = do
    fileName <- head <$> getArgs
    contents <- readFile fileName
    let allLines = lines contents
    let ints = map read allLines

    let answer1 = countIncreasing ints
    putStrLn $ "The first answer is: " ++ show answer1

    putStrLn "\n"

    let answer2 = countIncreasing $ getTotalWindow ints
    putStrLn $ "The second answer is: " ++ show answer2