module Main where
import System.Random
import System.IO
import Control.Monad (replicateM)
import Words 

handleGuesses :: Int -> String -> IO()
handleGuesses 0 word = putStrLn $ concat ["Word was ", word]
handleGuesses n word = do 
    guess <- getLine 
    if guess `elem` Words.wordList then
        if guess == word then
            putStrLn "Correct word"
        else 
            Words.getCharEqual (zip guess word) word >> handleGuesses (n-1) word
    else 
        putStrLn "Word is invalid" >> handleGuesses (n) word

main :: IO ()
main = do 
    number <- randomRIO(1,12972::Int)
    let word = (Words.getWords number Words.wordList)
    let guesses = 6
    putStrLn "======="
    handleGuesses guesses word