module Main where

main :: IO ()
main = do
    contents <- readFile "./data.txt"
    let allLines = lines contents
    let ints = map read allLines
    let answer1 = getTotalIncreasing ints
    putStrLn $ "The first answer is: " ++ show answer1

    let answer2 = getTotalIncreasing $ getTotalWindow ints
    putStrLn $ "The second answer is: " ++ show answer2

getTotalIncreasing :: [Int] -> Int
getTotalIncreasing list = countTrue $ getIncreasing list []

getIncreasing :: [Int] -> [Bool] -> [Bool]
getIncreasing [x,y] bools = bools ++ [y > x]
getIncreasing (x:y:xs) [] | y > x = getIncreasing (y:xs) [False, True]
                        | otherwise = getIncreasing (y:xs) [False, False]
getIncreasing (x:y:xs) bools | y > x = getIncreasing (y:xs) (bools ++ [True])
                            | otherwise = getIncreasing (y:xs) (bools ++ [False])
getIncreasing _ bools = bools

countTrue :: [Bool] -> Int
countTrue = length . filter (True ==)

getTotalWindow :: [Int] -> [Int]
getTotalWindow list = getWindow list []

getWindow :: [Int] -> [Int] -> [Int]
getWindow [x,y,z] windows = windows ++ [x+y+z]
getWindow (x:y:z:xs) windows = windows ++ [x+y+z] ++ getWindow (y:z:xs) windows
getWindow _ windows = windows