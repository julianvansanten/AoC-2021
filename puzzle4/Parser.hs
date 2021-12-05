{-# LANGUAGE TupleSections #-}
module Parser where
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.ParserCombinators.Parsec.Token as Token

-- EDSL for a bingo game
newtype CardCell = CardCell (Integer, Bool)
    deriving (Show, Eq)
newtype CardRow = CardRow [CardCell]
    deriving (Show, Eq)
newtype Card = Card [CardRow]
    deriving (Show, Eq)
newtype Draws = Draws [Integer]
    deriving (Show, Eq)
data BingoGame = BingoGame Draws [Card]
    deriving (Show, Eq)

-- A whitespace parser
spaces :: Parser ()
spaces = skipMany1 space

-- A parser for the list of ball draws
parseDraws :: Parser Draws
parseDraws = Draws . map read <$> x
    where x = many1 digit `sepBy1` (char ',' <* skipMany space)

parseCell :: Parser CardCell
parseCell = CardCell . (,False) . read <$> x
    where x = many (char ' ') *> many1 digit

-- A parser for a single row of a bingo card
parseRow :: Parser CardRow
parseRow = CardRow <$> x
    where x = parseCell `sepBy1` many1 (char ' ')

-- A parser for an entire bingo card
parseCard :: Parser Card
parseCard = Card <$> parseRow `sepEndBy1` newline

-- Parse a file to a BingoGame
parseFile :: Parser BingoGame
parseFile = BingoGame <$> parseDraws <* spaces <*> (parseCard `sepBy1` many newline)

-- Parse a String to a parsed BingoGame
getGame :: String -> Either ParseError BingoGame
getGame = parse parseFile ""