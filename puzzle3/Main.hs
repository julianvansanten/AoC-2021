module Main where
import System.Environment (getArgs)
import Data.Bits

-- Create a tuple of (zeros, ones) counting the occurrences in a given string
-- String must not contain any other characters than 0 or 1, function only checks
-- if a character is equal to zero.
countBits :: String -> (Integer, Integer)
countBits = foldl (\(z, o) s ->
    if s == '0'
        then (z + 1, o)
        else (z, o + 1))
        (0, 0)

-- Return the bit that occurs the most.
evalMSB :: (Integer, Integer) -> Integer
evalMSB (z, o)  | z > o     = 0
                | otherwise = 1

-- Get the most occuring bit from a string of ones and zeros.
getMSB :: String -> Integer
getMSB = evalMSB . countBits

-- Find all most significant bits for given strings containing ones and zeros.
getMSBits :: [String] -> [Integer]
getMSBits = map getMSB

-- Bit inverse of getMSBits
getLSBits :: [String] -> [Integer]
getLSBits = map ((\i -> if i == 0 then 1 else 0) . getMSB)

-- Calculate the number from the MS bits.
getNumber :: [Integer] -> Integer
getNumber i = sum $ zipWith (\a b -> b * (2^a)) [0..] r
    where r = reverse i

-- Matrix transpose to put each n-th bit in the same list
transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)


-- Exercise 2
-- Convert a string of bits to an integer. String should only contain zeros and ones,
-- as all characters that are not zero are counted as ones.
bitStringToInteger :: String -> Integer
bitStringToInteger [] = 0
bitStringToInteger (c:cs)   | c == '0' = bitStringToInteger cs
                            | otherwise = 2 ^ length cs + bitStringToInteger cs

-- Convert an entire array of strings to an array of integers.
bitArrayToInteger :: [String] -> [Integer]
bitArrayToInteger = map bitStringToInteger

-- Keep filtering numbers out of a list until either all numbers are
-- the same or the next bit filter results in an empty list.
bitwiseFilter :: [String] -> [String]
bitwiseFilter [] = []
bitwiseFilter xs = undefined


-- Filter an array with a bitwise or, where all bits should be zero.
falseFilter :: Integer -> [String] -> [String]
falseFilter n = filter (\ x -> let c = read x - 48 in (.&.) n c == 0)
-- Filter an array with a bitwise and, where all bits should be one.
trueFilter :: Integer -> [String] -> [String]
trueFilter n = filter (\ x -> let c = read x - 48 in (.&.) n c /= 0)


main :: IO ()
main = do
    fileName <- head <$> getArgs
    contents <- readFile fileName
    let allLines = lines contents
    let trans = transpose allLines
    let msb = getNumber $ getMSBits trans
    putStrLn $ "The gamma rate is: " ++ show msb
    let lsb = getNumber $ getLSBits trans
    putStrLn $ "The epsilon rate is: " ++ show lsb
    putStrLn $ "The answer to the first puzzle is: " ++ show (msb * lsb)

    -- let nums = bitArrayToInteger allLines
    -- print $ getMSBits trans
    -- print nums
    -- let oxygen = bitwiseFilter nums (getMSBits trans)
    -- putStrLn $ "The numbers remaining for the oxygen filter are: " ++ show oxygen
    -- let co2 = bitwiseFilter nums (getLSBits trans)
    -- putStrLn $ "The numbers remaining for the CO2 levels are: " ++ show co2