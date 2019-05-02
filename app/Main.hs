module Main where

import Lib

k:: [Int]
k = map (*2) [1,2,3]



filterM :: (a -> Bool) -> [a] -> [a]
filterM _ [] = []
filterM check (x:[]) = case check x of
                        True -> [x]
                        False -> []
filterM check (x:xs) = case check x of
                        True -> x : filterM check xs
                        False -> filterM check xs

main :: IO ()
main = do
        someFunc
        putStrLn "Kek"
        print $ filterM (== 2) [1,2,3]
        print $ filterM (\x -> mod x 2 == 0) [1,2,3]

