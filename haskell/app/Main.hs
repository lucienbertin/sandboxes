{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Schedule (ioNow)

-- main :: IO ()
-- main = do
--     now <- ioNow
--     putStrLn ("Hello, Haskell! it is now: " ++ show now)


main :: IO ()
main = scotty 3000 $
    get "/:word" $ do
        beam <- pathParam "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]