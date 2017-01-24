module Lib
    ( someFunc
    , chop
    ) where

-- Your code here!
-- Feel free to write helper functions.

-- chop :: [Int] -> [Int]

chop :: [Int] -> [Int]
chop [] = []
chop (x:xs) =   if x == 0
                then x : chop(xs)
                else (x - 1) : aux (xs)

aux [] = []
aux (x:xs) = x + (length xs + 2) : xs

someFunc :: IO ()
someFunc = putStrLn "someFunc"