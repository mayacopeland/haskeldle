module Main where
import System.Random
import System.IO
import Control.Monad (replicateM)
import Words 

handleGuesses :: Int -> IO()
handleGuesses 0 = return () 
handleGuesses n = do 
    guess <- getLine 
    if guess `elem` Words.wordList then
        handleGuesses (n-1)
    else 
        putStrLn "Word is invalid" >> handleGuesses (n)

main :: IO ()
main = do 
    number <- randomRIO(1,12972::Int)
    let word = (Words.getWords number Words.wordList)
    let guesses = 6
    handleGuesses guesses