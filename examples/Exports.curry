module Exports ( myFun ) where

myFun :: a -> a
myFun = id

myAlignmentMistake :: Int -> Int
myAlignmentMistake x | x == 0 = 0
                      | otherwise = 3