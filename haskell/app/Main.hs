module Main where

import Schedule (ioNow)

main :: IO ()
main = do
    now <- ioNow
    putStrLn ("Hello, Haskell! it is now: " ++ show now)
