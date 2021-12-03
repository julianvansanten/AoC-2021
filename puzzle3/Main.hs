module Main where
import System.Environment (getArgs)

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
evalMSB (z, o)  | z >= o    = 0
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