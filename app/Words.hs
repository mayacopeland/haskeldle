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

getCharEqual :: [(Char,Char)] -> String -> IO ()
getCharEqual ((x,y):[]) word
    | (x == y) = setSGR [SetColor Foreground Vivid Green] >>  putStr [x] >> putStrLn "" >> setSGR [SetColor Foreground Dull White] >> putStrLn "======="
    | (x `elem` word) = setSGR [SetColor Foreground Vivid Yellow] >> putStr [x] >> putStrLn "" >> setSGR [SetColor Foreground Dull White] >> putStrLn "======="
    | otherwise = setSGR [SetColor Foreground Vivid White] >> putStr [x] >> putStrLn "" >> setSGR [SetColor Foreground Dull White] >> putStrLn "=======" 
getCharEqual ((x,y):xs) word
    | (x == y) = setSGR [SetColor Foreground Vivid Green] >> putStr [x] >> getCharEqual xs word
    | (x `elem` word) = setSGR [SetColor Foreground Vivid Yellow] >> putStr [x] >> getCharEqual xs word
    | otherwise = setSGR [SetColor Foreground Vivid White] >> putStr [x] >> getCharEqual xs word