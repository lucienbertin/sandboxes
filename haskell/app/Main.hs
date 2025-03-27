module Main where

import Data.Time (LocalTime (LocalTime), dayOfWeek, DayOfWeek( Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday), TimeOfDay (TimeOfDay), getZonedTime, ZonedTime (ZonedTime))

data DailySchedule = Open | Closed | FromTo TimeOfDay TimeOfDay
data WeeklySchedule = Week DailySchedule DailySchedule DailySchedule DailySchedule DailySchedule DailySchedule DailySchedule -- 1 dailay schedule per day if the week

data Schedule = DailySchedule DailySchedule | WeeklySchedule WeeklySchedule

isOpenAt :: TimeOfDay -> DailySchedule -> Bool
isOpenAt _t Open = True
isOpenAt _t Closed = False
isOpenAt t (FromTo from to) = t >= from && t < to

dayOfWeekandTime :: LocalTime -> (DayOfWeek, TimeOfDay)
dayOfWeekandTime (LocalTime date time) = (dayOfWeek date, time)

isOpenOnDayOfWeek :: (DayOfWeek, TimeOfDay) -> WeeklySchedule -> Bool
isOpenOnDayOfWeek (Monday, t)    (Week mon _ _ _ _ _ _) = isOpenAt t mon
isOpenOnDayOfWeek (Tuesday, t)   (Week _ tue _ _ _ _ _) = isOpenAt t tue
isOpenOnDayOfWeek (Wednesday, t) (Week _ _ wed _ _ _ _) = isOpenAt t wed
isOpenOnDayOfWeek (Thursday, t)  (Week _ _ _ thu _ _ _) = isOpenAt t thu
isOpenOnDayOfWeek (Friday, t)    (Week _ _ _ _ fri _ _) = isOpenAt t fri
isOpenOnDayOfWeek (Saturday, t)  (Week _ _ _ _ _ sat _) = isOpenAt t sat
isOpenOnDayOfWeek (Sunday, t)    (Week _ _ _ _ _ _ sun) = isOpenAt t sun

isOpen :: LocalTime -> Schedule -> Bool
isOpen (LocalTime _date time) (DailySchedule s) = isOpenAt time s
isOpen datetime (WeeklySchedule week) = isOpenOnDayOfWeek (dayOfWeekandTime datetime) week

ioNow :: IO LocalTime
ioNow = fmap (\(ZonedTime now _tz) -> now) getZonedTime
isOpenNow :: Schedule -> IO Bool
isOpenNow s = fmap (`isOpen` s) ioNow

main :: IO ()
main = do
    now <- ioNow
    putStrLn ("Hello, Haskell! it is now: " ++ show now)

    let alwaysOpen = DailySchedule Open
    putStrLn ("this should be open: " ++ show (isOpen now alwaysOpen))
    let alwaysClosed = DailySchedule Closed
    putStrLn ("this should be closed: " ++ show (isOpen now alwaysClosed))
    let ntf = FromTo (TimeOfDay 9 0 0) (TimeOfDay 17 0 0)
    let nineToFive = DailySchedule ntf
    putStrLn ("this should be open between 9AM and 5PM: " ++ show (isOpen now nineToFive))

    let standardWorkingWeek = WeeklySchedule (Week Open Open Open Open Open Closed Closed)
    putStrLn ("this should be open on week days and closed on weekends: " ++ show (isOpen now standardWorkingWeek))
    let standardWorkingHours = WeeklySchedule (Week ntf ntf ntf ntf ntf Closed Closed)
    putStrLn ("this should be open on week days between 9AM and 5PM: " ++ show (isOpen now standardWorkingHours))
