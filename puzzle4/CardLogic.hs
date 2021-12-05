module CardLogic where
import Parser
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Monad.Trans.Except (except)

cardTranspose :: Card -> Card
cardTranspose (Card ((CardRow []):rs)) = Card []
cardTranspose (Card x) = h `combineRows` t
    where   h = CardRow (map rowHead x)
            t = cardTranspose $ Card (map rowTail x)

combineRows :: CardRow -> Card -> Card
combineRows r (Card rs) = Card (r:rs)

rowHead :: CardRow -> CardCell
rowHead (CardRow []) = error "Empty CardRow"
rowHead (CardRow (x:xs)) = x
rowTail :: CardRow -> CardRow
rowTail (CardRow []) = error "Empty CardRow"
rowTail (CardRow (x:xs)) = CardRow xs

cardHasBingo :: Card -> Bool
cardHasBingo (Card rs) = row || col
    where   rowChecks = foldl (\b r -> b || rowHasBingo r) False
            row = rowChecks rs
            (Card trans) = cardTranspose (Card rs)
            col = rowChecks trans

rowHasBingo :: CardRow -> Bool
rowHasBingo (CardRow cs) = foldl (\b (CardCell (_,cell)) -> b && cell) True cs

markCard :: Card -> Integer -> Card
markCard (Card c) i = Card (map (`markRow` i) c)

markRow :: CardRow -> Integer -> CardRow
markRow (CardRow r) i = CardRow (map (`markCell` i) r)

markCell :: CardCell -> Integer -> CardCell
markCell (CardCell(a, b)) i | a == i = CardCell (a, True)
                    | otherwise = CardCell (a, False)