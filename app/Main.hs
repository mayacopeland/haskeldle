module Main where
import System.Random
import System.IO
import Control.Monad (replicateM)
import Words 

handleGuesses :: Int -> IO()
handleGuesses 0 = return () 
handleGuesses n = do 
    guess <- getLine 
    handleGuesses (n-1)

main :: IO ()
main = do 
    number <- randomRIO(1,12972::Int)
    let word = (Words.getWords number Words.wordList)
    let guesses = 6
    handleGuesses guesses