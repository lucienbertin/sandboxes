module Schedule (
    DailySchedule (Open, Closed, FromTo),
    WeeklySchedule (Week),
    Schedule (DailySchedule, WeeklySchedule),

    ioNow, isOpen, isOpenNow
) where

import Data.Time (
    LocalTime (LocalTime),
    dayOfWeek, DayOfWeek( Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday),
    TimeOfDay (TimeOfDay),
    getZonedTime, ZonedTime (ZonedTime))

data DailySchedule = Open | Closed | FromTo TimeOfDay TimeOfDay
data WeeklySchedule = Week DailySchedule DailySchedule DailySchedule DailySchedule DailySchedule DailySchedule DailySchedule -- 1 daily schedule per day of the week, starts on monday

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