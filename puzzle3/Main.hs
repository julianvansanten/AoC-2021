module Main where
import System.Environment (getArgs)
import Data.Bits
import Data.Char

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
-- Filter a string of bits by applying a given function over each result
filterRecursiveBits :: ([String] -> [Integer]) -> [String] -> Int -> String
filterRecursiveBits f strs i = result
    where   msb = f (transpose strs)!!i
            filt = filterBit strs i msb
            result = if length filt <= 1 then head filt else filterRecursiveBits f filt (i + 1)

-- Filter a given bit from a given array of strings containing zeros and ones.
filterBit :: [String] -> Int -> Integer -> [String]
filterBit strs i b = filter (\s -> s!!i == bit) strs
    where bit = chr $ fromIntegral b + 48

-- Find the oxygen rating for a given array of bits.
findOxygenRating :: [String] -> Integer
findOxygenRating strs = bitConversion $ filterRecursiveBits getMSBits strs 0

-- Find the CO2 rating for a given array of bits.
findCO2Scrubbing :: [String] -> Integer
findCO2Scrubbing strs = bitConversion $ filterRecursiveBits getLSBits strs 0

-- Convert a string of bits to an Integer
bitConversion :: String -> Integer
bitConversion str = getNumber $ map (\c -> toInteger (ord c - 48)) str

-- Compute the answer to the second puzzle by calculating the oxygen rating, the CO2 rating and multiplying the results.
getSecondAnswer :: [String] -> Integer
getSecondAnswer strs = oxy * co2
    where   oxy = findOxygenRating strs
            co2 = findCO2Scrubbing strs


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

    let second = getSecondAnswer allLines
    putStrLn $ "The answer to the second puzzle is: " ++ show second