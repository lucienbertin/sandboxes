module Main where

import Data.Time (getCurrentTime)

main :: IO ()
main = do
    now <- fmap show getCurrentTime
    putStrLn ("Hello, Haskell! it is now: " ++ now)
