{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty (liftIO, ActionM, scotty, text, post, jsonData)
import Schedule (Schedule, isOpenNow)
import Data.Text.Lazy (pack)

main :: IO ()
main = scotty 3000 $
    post "/" $ do
        schedule <- jsonData :: ActionM Schedule -- given a schedule as json body
        open <- liftIO (isOpenNow schedule)
        text (pack (show open)) -- answers if its open now