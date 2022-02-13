module Main where
import System.Random
import System.IO
import Control.Monad (replicateM)
import Words 

handleGuesses :: Int -> String -> [String] -> IO()
handleGuesses 0 word wList = putStrLn $ concat ["Word was ", word]
handleGuesses n word wList = do 
    guess <- getLine 
    if guess `elem` wList then
        if guess == word then
            putStrLn "Correct word"
        else 
            Words.getCharEqual (zip guess word) word >> handleGuesses (n-1) word wList
    else 
        putStrLn "Word is invalid" >> handleGuesses (n) word wList

main :: IO ()
main = do 
    words <- readFile "words.txt"
    let wList = Words.wordList words
    number <- randomRIO(1,(length wList)::Int)
    let word = (Words.getWords number wList)
    let guesses = 6
    putStrLn "======="
    handleGuesses guesses word wList