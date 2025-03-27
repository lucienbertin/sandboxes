module Main where

import Data.Time (LocalTime (LocalTime), dayOfWeek, DayOfWeek( Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday), TimeOfDay (TimeOfDay), getZonedTime, ZonedTime (ZonedTime))

data DailySchedule = Open | Closed | FromTo TimeOfDay TimeOfDay
data WeeklySchedule = Week DailySchedule DailySchedule DailySchedule DailySchedule DailySchedule DailySchedule DailySchedule -- 1 dailay schedule per day if the week

data Schedule = DailySchedule DailySchedule | WeeklySchedule WeeklySchedule

isOpenAt :: DailySchedule -> TimeOfDay -> Bool
isOpenAt Open _= True
isOpenAt Closed _ = False
isOpenAt (FromTo from to) t = t >= from && t < to

data WeekTime = WeekTime DayOfWeek TimeOfDay
weekTime :: LocalTime -> WeekTime
weekTime (LocalTime date time) = WeekTime (dayOfWeek date) time

isOpenOnDayOfWeek :: WeeklySchedule -> WeekTime -> Bool
isOpenOnDayOfWeek (Week mon _ _ _ _ _ _) (WeekTime Monday    t) = isOpenAt mon t
isOpenOnDayOfWeek (Week _ tue _ _ _ _ _) (WeekTime Tuesday   t) = isOpenAt tue t
isOpenOnDayOfWeek (Week _ _ wed _ _ _ _) (WeekTime Wednesday t) = isOpenAt wed t
isOpenOnDayOfWeek (Week _ _ _ thu _ _ _) (WeekTime Thursday  t) = isOpenAt thu t
isOpenOnDayOfWeek (Week _ _ _ _ fri _ _) (WeekTime Friday    t) = isOpenAt fri t
isOpenOnDayOfWeek (Week _ _ _ _ _ sat _) (WeekTime Saturday  t) = isOpenAt sat t
isOpenOnDayOfWeek (Week _ _ _ _ _ _ sun) (WeekTime Sunday    t) = isOpenAt sun t

isOpen :: Schedule -> LocalTime -> Bool
isOpen (DailySchedule s) (LocalTime _ time)  = isOpenAt s time
isOpen (WeeklySchedule week) dateTime  = isOpenOnDayOfWeek week (weekTime dateTime) 

ioNow :: IO LocalTime
ioNow = fmap (\(ZonedTime now _tz) -> now) getZonedTime
isOpenNow :: Schedule -> IO Bool
isOpenNow s = fmap (isOpen s) ioNow

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
