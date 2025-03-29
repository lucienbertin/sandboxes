{-# LANGUAGE InstanceSigs #-}
module Schedule (
    DailySchedule (Open, Closed, FromTo, Switch),
    WeeklySchedule (Week), MondaysSchedule, TuesdaysSchedule, WednesdaysSchedule, ThursdaysSchedule, FridaysSchedule, SaturdaysSchedule, SundaysSchedule,
    YearlySchedule (Year), PartialYearSchedule (PartialYear),
    Schedule (DailySchedule, WeeklySchedule, YearlySchedule),

    isOpen, isClosed,
    ioNow, isOpenNow, isClosedNow) where

import Data.Time (
    LocalTime (LocalTime),
    dayOfWeek, DayOfWeek(Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday),
    TimeOfDay (TimeOfDay),
    getZonedTime, ZonedTime (ZonedTime),
    MonthOfYear, DayOfMonth, isLeapYear)

import Data.SortedList (SortedList, fromSortedList)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Calendar.MonthDay (monthAndDayToDayOfYear)


data DailySchedule =
      Open
    | Closed
    | FromTo TimeOfDay TimeOfDay
    | Switch (SortedList TimeOfDay) -- on each ToD of the list toggles between open and closed, starts closed
    deriving (Show)

type MondaysSchedule = DailySchedule
type TuesdaysSchedule = DailySchedule
type WednesdaysSchedule = DailySchedule
type ThursdaysSchedule = DailySchedule
type FridaysSchedule = DailySchedule
type SaturdaysSchedule = DailySchedule
type SundaysSchedule = DailySchedule

data WeeklySchedule = Week MondaysSchedule TuesdaysSchedule WednesdaysSchedule ThursdaysSchedule FridaysSchedule SaturdaysSchedule SundaysSchedule deriving (Show)

data PartialYearSchedule = PartialYear MonthOfYear DayOfMonth Schedule deriving (Show)
instance Eq PartialYearSchedule where
    PartialYear m d _ == PartialYear n e _ = m == n && d == e

comparePartialYearSchedule :: PartialYearSchedule -> PartialYearSchedule -> Ordering
comparePartialYearSchedule (PartialYear m d _) (PartialYear n e _) | m == n = compare d e
comparePartialYearSchedule (PartialYear m d _) (PartialYear n e _) = compare m n
instance Ord PartialYearSchedule where
  compare = comparePartialYearSchedule


data YearlySchedule = Year (SortedList PartialYearSchedule) deriving (Show)

data Schedule = DailySchedule DailySchedule | WeeklySchedule WeeklySchedule | YearlySchedule YearlySchedule deriving (Show)

isOpenAt :: DailySchedule -> TimeOfDay -> Bool
isOpenAt Open   _ = True
isOpenAt Closed _ = False
isOpenAt (FromTo from to) t = t >= from && t < to
isOpenAt (Switch s) t = (odd.length.filter (<= t)) (fromSortedList s)

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

partialYearScheduleIsApplicable (PartialYear moy dom _) isLeapYear = (<=) (monthAndDayToDayOfYear isLeapYear moy dom)
isOpenOnDayOfYear :: YearlySchedule -> LocalTime -> Bool
isOpenOnDayOfYear (Year schedules) dt = isOpen schedule dt
    where 
        lastSchedule = last (fromSortedList schedules) -- will be the one applied for the start of the year until another one match
        LocalTime d _ = dt
        (year, doy) = toOrdinalDate d
        applicableSchedules = filter (\s -> partialYearScheduleIsApplicable s (isLeapYear year) doy) (fromSortedList schedules)
        applicableSchedulesAndFallback = lastSchedule : applicableSchedules
        (PartialYear _ _ schedule) = last applicableSchedulesAndFallback

isOpen :: Schedule -> LocalTime -> Bool
isOpen (DailySchedule ds)  (LocalTime _ time) = isOpenAt ds time
isOpen (WeeklySchedule ws) dateTime           = isOpenOnDayOfWeek ws (weekTime dateTime)
isOpen (YearlySchedule ys)   dateTime           = isOpenOnDayOfYear ys dateTime

isClosed :: Schedule -> LocalTime -> Bool
isClosed s t = not (isOpen s t)

-- now
ioNow :: IO LocalTime
ioNow = fmap (\(ZonedTime now _tz) -> now) getZonedTime
isOpenNow :: Schedule -> IO Bool
isOpenNow s = fmap (isOpen s) ioNow
isClosedNow :: Schedule -> IO Bool
isClosedNow s = fmap (isClosed s) ioNow