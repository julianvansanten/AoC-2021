module Main where
import System.Environment (getArgs)
import Parser
import CardLogic

main = do
    fileName <- head <$> getArgs
    contents <- readFile fileName
    case getGame contents of
            Left err  -> print err
            Right xs  -> print xs