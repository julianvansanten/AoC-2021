{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module CardLogic where
import EDSL
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Monad.Trans.Except (except)

-- Transpose a BingoCard, used to check the columns for bingo.
cardTranspose :: Card -> Card
cardTranspose (Card ((CardRow []):rs)) = Card []
cardTranspose (Card x) = h `combineRows` t
    where   h = CardRow (map rowHead x)
            t = cardTranspose $ Card (map rowTail x)

-- Add a CardRow to the beginning of a Card. Has the same functionality as
-- (:) in lists.
combineRows :: CardRow -> Card -> Card
combineRows r (Card rs) = Card (r:rs)

-- Get the first CardCell of a CardRow.
-- Throws an error if a CardRow contains no CardCells.
rowHead :: CardRow -> CardCell
rowHead (CardRow []) = error "Empty CardRow"
rowHead (CardRow (x:xs)) = x
-- Get a list of CardCells without the first element of a given CardRow.
-- Throws an error if a CardRow contains no CardCells.
rowTail :: CardRow -> CardRow
rowTail (CardRow []) = error "Empty CardRow"
rowTail (CardRow (x:xs)) = CardRow xs

-- Get Cards with Bingo.
getBingos :: [Card] -> [Card]
getBingos = filter cardHasBingo

-- Check if a Card has bingo.
cardHasBingo :: Card -> Bool
cardHasBingo (Card rs) = row || col
    where   rowChecks = foldl (\b r -> b || rowHasBingo r) False
            row = rowChecks rs
            (Card trans) = cardTranspose (Card rs)
            col = rowChecks trans

-- Check if a CardRow has Bingo.
rowHasBingo :: CardRow -> Bool
rowHasBingo (CardRow cs) = foldl (\b (CardCell (_,cell)) -> b && cell) True cs

-- Mark a given number on if it exists on the Card.
-- Returns the same Card if no number is found on the Card.
markCard :: Card -> Integer -> Card
markCard (Card c) i = Card (map (`markRow` i) c)

-- Mark a number in a CardRow if it exists.
-- Returns the same CardRow if it does not contain the given number.
markRow :: CardRow -> Integer -> CardRow
markRow (CardRow r) i = CardRow (map (`markCell` i) r)

-- Mark a cell if it contains the given number with True, otherwise
-- keep unchanged.
markCell :: CardCell -> Integer -> CardCell
markCell (CardCell(a, b)) i | a == i = CardCell (a, True)
                    | otherwise = CardCell (a, b)


-- Find numbers
findCellValue :: CardCell -> Integer
findCellValue (CardCell (v, False)) = v
findCellValue (CardCell (_, True)) = 0

findRowSum :: CardRow -> Integer
findRowSum (CardRow cs) = sum $ map findCellValue cs

findCardSum :: Card -> Integer
findCardSum (Card rs) = sum $ map findRowSum rs

-- A Result can either be a list of cards that have Bingo along with the
-- last number that was drawn, or a new BingoGame with the last number
-- processed and a list of processed numbers.
type Result = Either BingoGame ([Card], Integer)

-- Find the winning card from a given BingoGame.
findWinningCards :: BingoGame -> Result
findWinningCards b = processDrawsForWin (Left b)

-- Run through Cards to find the first winning card.
processDrawsForWin :: Result -> Result
-- If a completed Result is entered, no need to continue.
processDrawsForWin (Right a) = Right a
-- If a BingoGame is not completed yet, process the next draw.
--  If a winning card is found after this draw, the result is found.
--  If this is not the case, continue on the next draw.
processDrawsForWin (Left (BingoGame (Draws (d:ds)) cs)) =
    if null bingos
        then processDrawsForWin $ Left (BingoGame (Draws ds) processedCards)
        else Right (bingos, d)
    where
        processedCards = map (`markCard` d) cs
        bingos = getBingos processedCards
-- if a BingoGame is out of draws, no winning card exists.
processDrawsForWin (Left (BingoGame (Draws []) _)) = error "No more draws!"


--Exercise 2
-- Run through Cards to find the last winning card.
findLosingCards :: BingoGame -> Result
findLosingCards b = processDrawsForLose (Left b)

-- On each run, filter out winning cards until the next filter
-- results in an empty list of ongoing cards.
processDrawsForLose :: Result -> Result
processDrawsForLose (Right a) = Right a
processDrawsForLose (Left (BingoGame (Draws (d:ds)) cs)) =
    if null remains
        then Right (processedCards, d)
        else processDrawsForLose $ Left (BingoGame (Draws ds) remains)
    where
        processedCards = map (`markCard` d) cs
        remains = filter (not . cardHasBingo) processedCards
processDrawsForLose (Left (BingoGame (Draws [d]) cs)) = Right (cs, d)
processDrawsForLose (Left (BingoGame (Draws []) _)) = error "No more draws!"
