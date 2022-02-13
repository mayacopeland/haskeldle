module Words (
    getWords 
    , wordList
    , getCharEqual
) where

import System.Console.ANSI

getWords :: Int -> [String] -> String 
getWords n xs = xs !! (n-1)

wordList :: String -> [String]
wordList words = lines words

notSeenPrior :: Char -> String -> String -> Bool
notSeenPrior y (x:[]) word = False
notSeenPrior y (x:xs) word
    | (y == x)  = True
    | otherwise = notSeenPrior y xs word

getCharEqual :: [(Char,Char)] -> String -> String -> IO ()
getCharEqual ((x,y):[]) word guess
    | (x == y) = setSGR [SetColor Foreground Vivid Green] >>  putStr [x] >> putStrLn "" >> setSGR [SetColor Foreground Dull White] >> putStrLn "======="
    | (x `elem` word) && not (notSeenPrior x guess guess) = setSGR [SetColor Foreground Vivid Yellow] >> putStr [x] >> putStrLn "" >> setSGR [SetColor Foreground Dull White] >> putStrLn "======="
    | otherwise = setSGR [SetColor Foreground Dull White] >> putStr [x] >> putStrLn "" >> setSGR [SetColor Foreground Dull White] >> putStrLn "=======" 
getCharEqual ((x,y):xs) word guess
    | (x == y) = setSGR [SetColor Foreground Vivid Green] >> putStr [x] >> getCharEqual xs word guess
    | (x `elem` word) = setSGR [SetColor Foreground Vivid Yellow] >> putStr [x] >> getCharEqual xs word guess
    | otherwise = setSGR [SetColor Foreground Dull White] >> putStr [x] >> getCharEqual xs word guess