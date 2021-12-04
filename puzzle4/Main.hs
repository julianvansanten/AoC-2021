module Main where
import System.Environment (getArgs)
import Parser
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.ParserCombinators.Parsec.Token as Token

cardTranspose :: Card -> Card
cardTranspose (Card rs) = undefined

rowHasBingo :: CardRow -> Bool
rowHasBingo (CardRow cs) = foldl (\b (CardCell (_,cell)) -> b && cell) True cs

markCard :: Card -> Integer -> Card
markCard (Card c) i = Card (map (`markRow` i) c)

markRow :: CardRow -> Integer -> CardRow
markRow (CardRow r) i = CardRow (map (`markCell` i) r)

markCell :: CardCell -> Integer -> CardCell
markCell (CardCell(a, b)) i | a == i = CardCell (a, True)
                    | otherwise = CardCell (a, False)

main = do
    fileName <- head <$> getArgs
    contents <- readFile fileName
    case parse parseFile "" contents of
            Left err  -> print err
            Right xs  -> print xs