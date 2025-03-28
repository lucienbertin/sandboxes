module Main where
import Test.HUnit
import qualified System.Exit as Exit

import Schedule (isOpen, isClosed, DailySchedule (Open, Closed, FromTo), WeeklySchedule (Week), Schedule (WeeklySchedule, DailySchedule))
import Data.Time (Day, TimeOfDay (TimeOfDay), LocalTime (LocalTime), midday, midnight)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)

-- date and times
monday =    fromOrdinalDate 2025 5 -- 1st complete week of 2025
tuesday =   fromOrdinalDate 2025 6
wednesday = fromOrdinalDate 2025 7
thursday =  fromOrdinalDate 2025 8
friday =    fromOrdinalDate 2025 9
saturday =  fromOrdinalDate 2025 10
sunday =    fromOrdinalDate 2025 11

sixAM =   TimeOfDay  6 0 0
nineAM =  TimeOfDay  9 0 0
fivePM =  TimeOfDay 17 0 0
eightPM = TimeOfDay 20 0 0

mondayAtNoon = LocalTime monday midday
mondayAtNight = LocalTime monday midnight

-- schedules
alwaysOpen = DailySchedule Open
alwaysClosed = DailySchedule Closed
nineToFive = FromTo nineAM fivePM
nineToFiveDaily = DailySchedule nineToFive
openOnWeekDays = WeeklySchedule (Week Open Open Open Open Open Closed Closed)
nineToFiveOnWeekDays = WeeklySchedule (Week nineToFive nineToFive nineToFive nineToFive nineToFive Closed Closed)

-- tests
alwaysOpenOnMondays :: Test
alwaysOpenOnMondays = TestCase (assertBool "should be open on any date" (isOpen alwaysOpen mondayAtNoon))
alwaysClosedOnMondays :: Test
alwaysClosedOnMondays = TestCase (assertBool "should be close on any date" (isClosed alwaysClosed mondayAtNoon))
 
tests :: Test
tests = TestList [
    TestLabel "alwaysOpenOnMondays" alwaysOpenOnMondays,
    TestLabel "alwaysClosedOnMondays" alwaysClosedOnMondays]
 
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
 