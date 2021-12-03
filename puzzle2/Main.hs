module Main where
import System.Environment ( getArgs )
import Data.Either
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Exercise 1
-- The coordinates are represented as an integer tuple, with horizontal being the first parameter
-- and vertical being the second parameter
type Coordinates = (Integer, Integer)

-- The directions as a datatype
data Direction = Forward Integer | Down Integer | Up Integer
    deriving (Eq, Show)

-- Strings for tokenization
languageDef = emptyDef {
    Token.reservedNames = ["forward", "up", "down"]
}

-- Initiating ParSec expressions
lexer = Token.makeTokenParser languageDef
integer = Token.integer lexer
direction = Token.reserved lexer

-- Direction parsers
parseUp :: Parser Direction
parseUp = Up <$> (direction "up" *> integer)
parseDown :: Parser Direction
parseDown = Down <$> (direction "down" *> integer)
parseForward :: Parser Direction
parseForward = Forward <$> (direction "forward" *> integer)
parseDirection :: Parser Direction
parseDirection = parseUp <|> parseDown <|> parseForward

fromLeft' :: Either l r -> l
fromLeft' (Left x) = x

fromRight' :: Either l r -> r
fromRight' (Right x) = x

parser :: Parser a -> String -> a
parser p xs | isLeft res = error $ show $ fromLeft' res
          | otherwise  = fromRight' res
  where res = parse p "" xs

-- Parse a single direction
parserDirection :: String -> Direction
parserDirection = parser parseDirection

getDirections :: [String] -> [Direction]
getDirections = map parserDirection

-- Go from a list of directions and a given coordinate to a new coordinate
getCoordinates :: [Direction] -> Coordinates -> Coordinates
getCoordinates [] c = c
getCoordinates (Forward a:xs) (x,y) = getCoordinates xs (x + a, y)
getCoordinates (Up a:xs) (x,y) = getCoordinates xs (x, y - a)
getCoordinates (Down a:xs) (x,y) = getCoordinates xs (x, y + a)

-- The final answer is the two coordinates multiplied together
calculateFirstAnswer :: Coordinates -> Integer
calculateFirstAnswer (x, y) = x * y

-- Multiply the coordinates
findLocation :: [String] -> Coordinates
findLocation = (\x -> getCoordinates x (0,0)) . getDirections


--Exercise 2
-- Create a new type for the coordinates and the aim
type Aim = (Coordinates, Integer)
-- Show what each direction does
getCoordinatesAim :: [Direction] -> Aim -> Aim
getCoordinatesAim [] a = a
getCoordinatesAim (Down b : bs) (c, a) = getCoordinatesAim bs (c, a + b)
getCoordinatesAim (Up b : bs) (c, a) = getCoordinatesAim bs (c, a - b)
getCoordinatesAim (Forward b : bs) ((x,y), a) = getCoordinatesAim bs ((x + b, y + mult), a)
    where mult = a * b

-- Calculate the multiplication of the two coordinates
calculateSecondAnswer :: Aim -> Integer
calculateSecondAnswer (c, _) = calculateFirstAnswer c

-- Find the coordinates and the aim
findAimLocation :: [String] -> Aim
findAimLocation = (\x -> getCoordinatesAim x ((0,0), 0)) . getDirections

main :: IO ()
main = do
    fileName <- head <$> getArgs
    contents <- readFile fileName
    let allLines = lines contents
    let coordinates = findLocation allLines
    putStrLn $ "The final coordinates are: " ++ show coordinates
    let totalMult = calculateFirstAnswer coordinates
    putStrLn $ "Multiplying them results in: " ++ show totalMult

    let aim = findAimLocation allLines
    putStrLn $ "The second coordinates and aim are: " ++ show aim
    let totalAim = calculateSecondAnswer aim
    putStrLn $ "Multiplying the coordinates results in: " ++ show totalAim