module Main where

import Data.Time (LocalTime (LocalTime), dayOfWeek, DayOfWeek( Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday), TimeOfDay (TimeOfDay), getZonedTime, ZonedTime (ZonedTime))

-- newtype FixedSchedule = Fixed Bool
data DailySchedule = Open | Closed | FromTo TimeOfDay TimeOfDay
data WeeklySchedule = Week {
    mon :: DailySchedule,
    tue :: DailySchedule,
    wed :: DailySchedule,
    thu :: DailySchedule,
    fri :: DailySchedule,
    sat :: DailySchedule,
    sun :: DailySchedule
}

data Schedule = DailySchedule DailySchedule | WeeklySchedule WeeklySchedule

isOpenAt :: TimeOfDay -> DailySchedule -> Bool
isOpenAt _t Open = True
isOpenAt _t Closed = False
isOpenAt t (FromTo from to) = t >= from && t < to

dayOfWeekandTime :: LocalTime -> (DayOfWeek, TimeOfDay)

dayOfWeekandTime (LocalTime date time) = (dayOfWeek date, time)
isOpenOnDayOfWeek :: (DayOfWeek, TimeOfDay) -> WeeklySchedule -> Bool
isOpenOnDayOfWeek (Monday, t) week = isOpenAt t (mon week)
isOpenOnDayOfWeek (Tuesday, t) week = isOpenAt t (tue week)
isOpenOnDayOfWeek (Wednesday, t) week = isOpenAt t (wed week)
isOpenOnDayOfWeek (Thursday, t) week = isOpenAt t (thu week)
isOpenOnDayOfWeek (Friday, t) week = isOpenAt t (fri week)
isOpenOnDayOfWeek (Saturday, t) week = isOpenAt t (sat week)
isOpenOnDayOfWeek (Sunday, t) week = isOpenAt t (sun week)

isOpen :: LocalTime -> Schedule -> Bool
isOpen (LocalTime _date time) (DailySchedule s) = isOpenAt time s
isOpen datetime (WeeklySchedule week) = isOpenOnDayOfWeek (dayOfWeekandTime datetime) week

ioNow :: IO LocalTime
ioNow = fmap (\(ZonedTime now _tz) -> now) getZonedTime
isOpenNow :: Schedule -> IO Bool
isOpenNow s = fmap (`isOpen` s) ioNow

main :: IO ()
main = do
    ZonedTime now _tz <- getZonedTime
    let nowStr = show now
    let alwaysOpen = DailySchedule Open
    let alwaysClosed = DailySchedule Closed
    let ntf = FromTo (TimeOfDay 9 0 0) (TimeOfDay 17 0 0)
    let nineToFive = DailySchedule ntf
    let standardWorkingWeek = WeeklySchedule (Week {
        mon = Open,
        tue = Open,
        wed = Open,
        thu = Open,
        fri = Open,
        sat = Closed,
        sun = Closed
    })
    let standardWorkingHours = WeeklySchedule (Week {
        mon = ntf,
        tue = ntf,
        wed = ntf,
        thu = ntf,
        fri = ntf,
        sat = Closed,
        sun = Closed
    })
    putStrLn ("Hello, Haskell! it is now: " ++ nowStr)
    putStrLn ("this should be open: " ++ show (isOpen now alwaysOpen))
    putStrLn ("this should be closed: " ++ show (isOpen now alwaysClosed))
    putStrLn ("this should be open on standard working day: " ++ show (isOpen now standardWorkingWeek))
    putStrLn ("this should be open between 9AM and 5PM: " ++ show (isOpen now nineToFive))
    putStrLn ("this should be open in weekday between 9AM and 5PM: " ++ show (isOpen now standardWorkingHours))
