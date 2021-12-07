module EDSL where

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