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
threePM = TimeOfDay 15 0 0
fivePM =  TimeOfDay 17 0 0
eightPM = TimeOfDay 20 0 0

mondayAt6AM = LocalTime monday sixAM
mondayAt9AM = LocalTime monday nineAM
mondayAtNoon = LocalTime monday midday
mondayAt3PM = LocalTime monday threePM
mondayAt5PM = LocalTime monday fivePM
mondayAt8PM = LocalTime monday eightPM
mondayAtNight = LocalTime monday midnight


-- schedules
alwaysOpen = DailySchedule Open
alwaysClosed = DailySchedule Closed
nineToFive = FromTo nineAM fivePM
nineToFiveDaily = DailySchedule nineToFive
openOnWeekDays = WeeklySchedule (Week Open Open Open Open Open Closed Closed)
nineToFiveOnWeekDays = WeeklySchedule (Week nineToFive nineToFive nineToFive nineToFive nineToFive Closed Closed)

-- tests
alwaysOpenOnMondays = TestCase (assertBool "should be open on any date" (isOpen alwaysOpen mondayAtNoon))
alwaysClosedOnMondays = TestCase (assertBool "should be close on any date" (isClosed alwaysClosed mondayAtNoon))
openDuringWorkingHours1 = TestCase (assertBool "should be open @9AM" (isOpen nineToFiveDaily mondayAtNoon))
openDuringWorkingHours2 = TestCase (assertBool "should be open @noon" (isOpen nineToFiveDaily mondayAtNoon))
openDuringWorkingHours3 = TestCase (assertBool "should be open @3PM" (isOpen nineToFiveDaily mondayAtNoon))
closedOutsideWorkingHours1 = TestCase (assertBool "should be closed @midnight" (isClosed nineToFiveDaily mondayAtNight))
closedOutsideWorkingHours2 = TestCase (assertBool "should be closed @6AM" (isClosed nineToFiveDaily mondayAt6AM))
closedOutsideWorkingHours3 = TestCase (assertBool "should be closing @5PM" (isClosed nineToFiveDaily mondayAt5PM))
closedOutsideWorkingHours4 = TestCase (assertBool "should be closed @8PM" (isClosed nineToFiveDaily mondayAt8PM))
 
tests :: Test
tests = TestList [
    TestLabel "alwaysOpenOnMondays" alwaysOpenOnMondays,
    TestLabel "alwaysClosedOnMondays" alwaysClosedOnMondays,
    TestLabel "openDuringWorkingHours1" openDuringWorkingHours1,
    TestLabel "openDuringWorkingHours2" openDuringWorkingHours2,
    TestLabel "openDuringWorkingHours3" openDuringWorkingHours3,
    TestLabel "closedOutsideWorkingHours1" closedOutsideWorkingHours1,
    TestLabel "closedOutsideWorkingHours2" closedOutsideWorkingHours2,
    TestLabel "closedOutsideWorkingHours3" closedOutsideWorkingHours3,
    TestLabel "closedOutsideWorkingHours4" closedOutsideWorkingHours4
    ]
 
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
 