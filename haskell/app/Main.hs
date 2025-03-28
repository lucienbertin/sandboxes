module Main where

import Data.Time (TimeOfDay (TimeOfDay))

import Schedule (ioNow, isOpen, DailySchedule (Open, Closed, FromTo), WeeklySchedule (Week), Schedule (WeeklySchedule, DailySchedule))

main :: IO ()
main = do
    now <- ioNow
    putStrLn ("Hello, Haskell! it is now: " ++ show now)

    let alwaysOpen = DailySchedule Open
    putStrLn ("this should be open: " ++ show (isOpen alwaysOpen now))
    let alwaysClosed = DailySchedule Closed
    putStrLn ("this should be closed: " ++ show (isOpen alwaysClosed now))
    let ntf = FromTo (TimeOfDay 9 0 0) (TimeOfDay 17 0 0)
    let nineToFive = DailySchedule ntf
    putStrLn ("this should be open between 9AM and 5PM: " ++ show (isOpen nineToFive now))

    let standardWorkingWeek = WeeklySchedule (Week Open Open Open Open Open Closed Closed)
    putStrLn ("this should be open on week days and closed on weekends: " ++ show (isOpen standardWorkingWeek now))
    let standardWorkingHours = WeeklySchedule (Week ntf ntf ntf ntf ntf Closed Closed)
    putStrLn ("this should be open on week days between 9AM and 5PM: " ++ show (isOpen standardWorkingHours now))
