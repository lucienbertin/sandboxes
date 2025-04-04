module Schedule (
    DailySchedule (Open, Closed, FromTo, Switch),
    WeeklySchedule (Week), MondaysSchedule, TuesdaysSchedule, WednesdaysSchedule, ThursdaysSchedule, FridaysSchedule, SaturdaysSchedule, SundaysSchedule,
    YearlySchedule (Year), PartialYearSchedule (PartialYear),
    RepeatingDaysSchedule (Repeat), ReferenceDay, DaysPattern,
    ExceptionalSchedule (RegularExceptBetween), BetweenSchedule (Between), RegularSchedule,
    AmendedSchedule (Amended), InitialSchedule, Amendment (Amend), DateOfEffect,
    Schedule (DailySchedule, WeeklySchedule, RepeatingDaysSchedule, YearlySchedule, ExceptionalSchedule, AmendedSchedule),

    isOpen, isClosed,
    ioNow, isOpenNow, isClosedNow) where

import Data.Time (
    LocalTime (LocalTime),
    dayOfWeek, DayOfWeek(Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday),
    TimeOfDay (TimeOfDay),
    getZonedTime, ZonedTime (ZonedTime),
    MonthOfYear, DayOfMonth, isLeapYear, Day, diffDays)

import Data.SortedList (SortedList, fromSortedList)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Calendar.MonthDay (monthAndDayToDayOfYear)
import Data.Ix (Ix(index))

-- daily schedule
data DailySchedule =
      Open
    | Closed
    | FromTo TimeOfDay TimeOfDay
    | Switch (SortedList TimeOfDay) -- on each ToD of the list toggles between open and closed, starts closed
    deriving (Show)

isOpenAt :: DailySchedule -> TimeOfDay -> Bool
isOpenAt Open   _ = True
isOpenAt Closed _ = False
isOpenAt (FromTo from to) t = t >= from && t < to
isOpenAt (Switch s) t = (odd.length.filter (<= t)) (fromSortedList s)

-- weekly schedule
type MondaysSchedule = DailySchedule
type TuesdaysSchedule = DailySchedule
type WednesdaysSchedule = DailySchedule
type ThursdaysSchedule = DailySchedule
type FridaysSchedule = DailySchedule
type SaturdaysSchedule = DailySchedule
type SundaysSchedule = DailySchedule

data WeeklySchedule = Week MondaysSchedule TuesdaysSchedule WednesdaysSchedule ThursdaysSchedule FridaysSchedule SaturdaysSchedule SundaysSchedule deriving (Show)

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

-- repeating days schedule
type ReferenceDay = Day
type DaysPattern = [DailySchedule] -- try add constraint length >= 1
data RepeatingDaysSchedule = Repeat ReferenceDay DaysPattern deriving (Show)

isOpenAccordingToPattern :: RepeatingDaysSchedule -> LocalTime -> Bool
isOpenAccordingToPattern (Repeat refDay pattern) (LocalTime day time) = isOpenAt applicableSchedule time where
    applicableSchedule = pattern !! index
    index = fromInteger $ rem diff patternLen
    diff = diffDays day refDay
    patternLen = toInteger $ length pattern

-- yearly schedule
data PartialYearSchedule = PartialYear MonthOfYear DayOfMonth Schedule deriving (Show)
instance Eq PartialYearSchedule where
    PartialYear m d _ == PartialYear n e _ = m == n && d == e

comparePartialYearSchedule :: PartialYearSchedule -> PartialYearSchedule -> Ordering
comparePartialYearSchedule (PartialYear m d _) (PartialYear n e _) | m == n = compare d e
comparePartialYearSchedule (PartialYear m d _) (PartialYear n e _) = compare m n
instance Ord PartialYearSchedule where
  compare = comparePartialYearSchedule

newtype YearlySchedule = Year (SortedList PartialYearSchedule) deriving (Show)

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

-- Exceptional Schedule
data BetweenSchedule = Between LocalTime LocalTime Schedule deriving (Show) -- end day excluded
type RegularSchedule = Schedule
data ExceptionalSchedule = RegularExceptBetween RegularSchedule [BetweenSchedule] deriving (Show)

isOpenOnDay :: ExceptionalSchedule -> LocalTime -> Bool
isOpenOnDay (RegularExceptBetween rs ex) dt = isOpen applicableSchedule dt where
    applicableExceptions = filter (\(Between start end _) -> dt >= start && dt < end) ex
    applicableSchedules = rs : fmap (\(Between _ _ s) -> s) applicableExceptions
    applicableSchedule = last applicableSchedules

-- Amended Schedule
type DateOfEffect = LocalTime
data Amendment = Amend DateOfEffect Schedule deriving (Show)
instance Eq Amendment where
    Amend d _ == Amend e _ = d == e
compareAmendments :: Amendment -> Amendment -> Ordering
compareAmendments (Amend d _) (Amend e _) = compare d e
instance Ord Amendment where
  compare = compareAmendments
type InitialSchedule = Schedule
data AmendedSchedule = Amended InitialSchedule (SortedList Amendment) deriving (Show)

isOpenWithAmendments :: AmendedSchedule -> LocalTime -> Bool
isOpenWithAmendments (Amended initialSchedule amendments) dt = isOpen applicableSchedule dt where
    applicableAmendments = filter (\(Amend doe _) -> dt >= doe) (fromSortedList amendments)
    applicableSchedules = initialSchedule : fmap (\(Amend _ s) -> s) applicableAmendments
    applicableSchedule = last applicableSchedules

-- all schedules
data Schedule =
      DailySchedule DailySchedule
    | WeeklySchedule WeeklySchedule
    | RepeatingDaysSchedule RepeatingDaysSchedule
    | YearlySchedule YearlySchedule
    | ExceptionalSchedule ExceptionalSchedule
    | AmendedSchedule AmendedSchedule deriving (Show)

isOpen :: Schedule -> LocalTime -> Bool
isOpen (DailySchedule ds) (LocalTime _ time) = isOpenAt ds time
isOpen (WeeklySchedule ws) dateTime          = isOpenOnDayOfWeek ws (weekTime dateTime)
isOpen (RepeatingDaysSchedule rds) dateTime  = isOpenAccordingToPattern rds dateTime
isOpen (YearlySchedule ys) dateTime          = isOpenOnDayOfYear ys dateTime
isOpen (ExceptionalSchedule es) dateTime     = isOpenOnDay es dateTime
isOpen (AmendedSchedule as) dateTime         = isOpenWithAmendments as dateTime

isClosed :: Schedule -> LocalTime -> Bool
isClosed s t = not (isOpen s t)

-- now
ioNow :: IO LocalTime
ioNow = fmap (\(ZonedTime now _tz) -> now) getZonedTime
isOpenNow :: Schedule -> IO Bool
isOpenNow s = fmap (isOpen s) ioNow
isClosedNow :: Schedule -> IO Bool
isClosedNow s = fmap (isClosed s) ioNow