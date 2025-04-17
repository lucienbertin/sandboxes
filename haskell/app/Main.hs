{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Schedule (ioNow, Amendment (Amend), Schedule (AmendedSchedule, DailySchedule, YearlySchedule), AmendedSchedule (Amended), DailySchedule (Closed, FromTo), YearlySchedule (Year), toSortedList, PartialYearSchedule (PartialYear), isOpenNow)
import Data.Time (LocalTime(LocalTime), TimeOfDay (TimeOfDay), midday)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
-- import Data.Aeson (encode)
import Data.Text.Lazy (pack)

main :: IO ()
main = scotty 3000 $
    post "/" $ do
        schedule <- jsonData :: ActionM Schedule -- given a schedule as json body
        open <- liftIO (isOpenNow schedule)
        text (pack (show open)) -- answers if its open now