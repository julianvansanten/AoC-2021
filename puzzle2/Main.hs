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

findLocation :: [String] -> Coordinates
findLocation = (\x -> getCoordinates x (0,0)) . getDirections

main :: IO ()
main = do
    fileName <- head <$> getArgs
    contents <- readFile fileName
    let coordinates = findLocation $ lines contents
    putStrLn $ "The final coordinates are: " ++ show coordinates

    let totalMult = calculateFirstAnswer coordinates
    putStrLn $ "Multiplying them results in: " ++ show totalMult