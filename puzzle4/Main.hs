module Main where
import System.Environment ( getArgs )
import Parser ( getGame )
import CardLogic
import EDSL

-- Calculate the sum of remaining numbers and multiply by the last number.
calculateAnswer :: Result -> Integer
calculateAnswer (Left _) = error "Final result not found!"
calculateAnswer (Right (bs, i)) = findCardSum (head bs) * i

-- Get a BingoGame from a given String
getBingoGame :: String -> BingoGame
getBingoGame str = do
    let x = getGame str
    case x of
        (Left err) -> error "Unable to parse given String!"
        (Right game) -> game

-- Parse a given String and calculate the value for the winning Card.
runFirst :: BingoGame -> Integer
runFirst game = calculateAnswer $ findWinningCards game


runSecond :: BingoGame -> Integer
runSecond game = calculateAnswer $ findLosingCards game

main = do
    fileName <- head <$> getArgs
    contents <- readFile fileName
    let game = getBingoGame contents
    let win = runFirst game
    putStrLn $ "The answer to the first part is: " ++ show win

    let lose = runSecond game
    putStrLn $ "The answer to the second part is: " ++ show lose