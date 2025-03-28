module Main where
import Test.HUnit
import qualified System.Exit as Exit

import Schedule (isOpen, isClosed, DailySchedule (Open, Closed, FromTo, Switch), WeeklySchedule (Week), Schedule (WeeklySchedule, DailySchedule))
import Data.Time (Day, TimeOfDay (TimeOfDay), LocalTime (LocalTime), midday, midnight)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.SortedList (toSortedList)

-- date and times
monday =    fromOrdinalDate 2025 6 -- 1st complete week of 2025
tuesday =   fromOrdinalDate 2025 7
wednesday = fromOrdinalDate 2025 8
thursday =  fromOrdinalDate 2025 9
friday =    fromOrdinalDate 2025 10
saturday =  fromOrdinalDate 2025 11
sunday =    fromOrdinalDate 2025 12

sixAM =   TimeOfDay  6 0 0
nineAM = TimeOfDay  9 0 0
onePM = TimeOfDay 13 0 0
twelveThirtyPM = TimeOfDay 12 30 0
threePM = TimeOfDay 15 0 0
fivePM =  TimeOfDay 17 0 0
eightPM = TimeOfDay 20 0 0

mondayAt6AM = LocalTime monday sixAM
mondayAt9AM = LocalTime monday nineAM
mondayAtNoon = LocalTime monday midday
mondayAtTwelveThirtyPM = LocalTime monday twelveThirtyPM
mondayAt3PM = LocalTime monday threePM
mondayAt5PM = LocalTime monday fivePM
mondayAt8PM = LocalTime monday eightPM
mondayAtNight = LocalTime monday midnight

tuesdayAt6AM = LocalTime tuesday sixAM
tuesdayAt3PM = LocalTime tuesday threePM
wednesdayAt9AM = LocalTime wednesday nineAM
wednesdayAtNight = LocalTime wednesday midnight
thursdayAtNoon = LocalTime thursday midday
thursdayAt8PM = LocalTime thursday eightPM
fridayAt3PM = LocalTime friday threePM
fridayAtNight = LocalTime friday midnight
saturdayAt9AM = LocalTime saturday nineAM
sundayAtNoon = LocalTime sunday midday

-- schedules
alwaysOpen = DailySchedule Open
alwaysClosed = DailySchedule Closed
nineToFive = FromTo nineAM fivePM
nineToFiveWithLunchbreak = Switch (toSortedList [nineAM, midday, onePM, fivePM])
nineToFiveDaily = DailySchedule nineToFive
nineToFiveWithLunchbreakDaily = DailySchedule nineToFiveWithLunchbreak
openOnWeekDays = WeeklySchedule (Week Open Open Open Open Open Closed Closed)
nineToFiveOnWeekDays = WeeklySchedule (Week nineToFive nineToFive nineToFive nineToFive nineToFive Closed Closed)



-- tests
-- daily schedules
dsAlwaysOpenOnMondays = TestCase (assertBool "should be open on any date" (isOpen alwaysOpen mondayAtNoon))
dsAlwaysClosedOnMondays = TestCase (assertBool "should be close on any date" (isClosed alwaysClosed mondayAtNoon))
-- FromTo
dsOpenDuringWorkingHours1 = TestCase (assertBool "should be opening @9AM" (isOpen nineToFiveDaily mondayAt9AM))
dsOpenDuringWorkingHours2 = TestCase (assertBool "should be open @noon" (isOpen nineToFiveDaily mondayAtNoon))
dsOpenDuringWorkingHours3 = TestCase (assertBool "should be open @12:30" (isOpen nineToFiveDaily mondayAtTwelveThirtyPM))
dsOpenDuringWorkingHours4 = TestCase (assertBool "should be open @3PM" (isOpen nineToFiveDaily mondayAt3PM))
dsClosedOutsideWorkingHours1 = TestCase (assertBool "should be closed @midnight" (isClosed nineToFiveDaily mondayAtNight))
dsClosedOutsideWorkingHours2 = TestCase (assertBool "should be closed @6AM" (isClosed nineToFiveDaily mondayAt6AM))
dsClosedOutsideWorkingHours3 = TestCase (assertBool "should be closing @5PM" (isClosed nineToFiveDaily mondayAt5PM))
dsClosedOutsideWorkingHours4 = TestCase (assertBool "should be closed @8PM" (isClosed nineToFiveDaily mondayAt8PM))
-- switch
dsOpenDuringWorkingHoursLunchBreak1 = TestCase (assertBool "should be opening @9AM" (isOpen nineToFiveWithLunchbreakDaily mondayAt9AM))
dsOpenDuringWorkingHoursLunchBreak2 = TestCase (assertBool "should be closing @noon" (isClosed nineToFiveWithLunchbreakDaily mondayAtNoon))
dsOpenDuringWorkingHoursLunchBreak3 = TestCase (assertBool "should be closed @3PM" (isClosed nineToFiveWithLunchbreakDaily mondayAtTwelveThirtyPM))
dsOpenDuringWorkingHoursLunchBreak4 = TestCase (assertBool "should be open @3PM" (isOpen nineToFiveWithLunchbreakDaily mondayAt3PM))
dsClosedOutsideWorkingHoursLunchBreak1 = TestCase (assertBool "should be closed @midnight" (isClosed nineToFiveWithLunchbreakDaily mondayAtNight))
dsClosedOutsideWorkingHoursLunchBreak2 = TestCase (assertBool "should be closed @6AM" (isClosed nineToFiveWithLunchbreakDaily mondayAt6AM))
dsClosedOutsideWorkingHoursLunchBreak3 = TestCase (assertBool "should be closing @5PM" (isClosed nineToFiveWithLunchbreakDaily mondayAt5PM))
dsClosedOutsideWorkingHoursLunchBreak4 = TestCase (assertBool "should be closed @8PM" (isClosed nineToFiveWithLunchbreakDaily mondayAt8PM))

-- weekly schedules
wsOpenOnMondays1 = TestCase (assertBool "should be open on a monday" (isOpen openOnWeekDays mondayAtNoon))
wsOpenOnMondays2 = TestCase (assertBool "should be open on a monday" (isOpen openOnWeekDays mondayAt8PM))
wsOpenOnTuesdays1 = TestCase (assertBool "should be open on a tuesday" (isOpen openOnWeekDays tuesdayAt6AM))
wsOpenOnTuesdays2 = TestCase (assertBool "should be open on a tuesday" (isOpen openOnWeekDays tuesdayAt3PM))
wsOpenOnWednesdays1 = TestCase (assertBool "should be open on a wednesday" (isOpen openOnWeekDays wednesdayAt9AM))
wsOpenOnWednesdays2 = TestCase (assertBool "should be open on a wednesday" (isOpen openOnWeekDays wednesdayAtNight))
wsOpenOnThursday1 = TestCase (assertBool "should be open on a thursday" (isOpen openOnWeekDays thursdayAtNoon))
wsOpenOnThursday2 = TestCase (assertBool "should be open on a thursday" (isOpen openOnWeekDays thursdayAt8PM))
wsOpenOnFridays1 = TestCase (assertBool "should be open on a friday" (isOpen openOnWeekDays fridayAt3PM))
wsOpenOnFridays2 = TestCase (assertBool "should be open on a friday" (isOpen openOnWeekDays fridayAtNight))
wsOpenOnSaturdays1 = TestCase (assertBool "should be closed on a saturday" (isClosed openOnWeekDays saturdayAt9AM))
wsOpenOnSundays1 = TestCase (assertBool "should be closed on a sunday" (isClosed openOnWeekDays sundayAtNoon))

wsNtFOpenOnMondays1 = TestCase (assertBool "should be open on a monday at noon" (isOpen nineToFiveOnWeekDays mondayAtNoon))
wsNtFOpenOnMondays2 = TestCase (assertBool "should be closed on a monday at 8PM" (isClosed nineToFiveOnWeekDays mondayAt8PM))
wsNtFOpenOnTuesdays1 = TestCase (assertBool "should be closed on a tuesday at 6AM" (isClosed nineToFiveOnWeekDays tuesdayAt6AM))
wsNtFOpenOnTuesdays2 = TestCase (assertBool "should be open on a tuesday at 3PM" (isOpen nineToFiveOnWeekDays tuesdayAt3PM))
wsNtFOpenOnWednesdays1 = TestCase (assertBool "should be open on a wednesday at 9AM" (isOpen nineToFiveOnWeekDays wednesdayAt9AM))
wsNtFOpenOnWednesdays2 = TestCase (assertBool "should be closed on a wednesday at midnight" (isClosed nineToFiveOnWeekDays wednesdayAtNight))
wsNtFOpenOnThursday1 = TestCase (assertBool "should be open on a thursday at noon" (isOpen nineToFiveOnWeekDays thursdayAtNoon))
wsNtFOpenOnThursday2 = TestCase (assertBool "should be closed on a thursday at 8PM" (isClosed nineToFiveOnWeekDays thursdayAt8PM))
wsNtFOpenOnFridays1 = TestCase (assertBool "should be open on a friday at 3PM" (isOpen nineToFiveOnWeekDays fridayAt3PM))
wsNtFOpenOnFridays2 = TestCase (assertBool "should be closed on a friday at midnignt" (isClosed nineToFiveOnWeekDays fridayAtNight))
wsNtFOpenOnSaturdays1 = TestCase (assertBool "should be closed on a saturday" (isClosed nineToFiveOnWeekDays saturdayAt9AM))
wsNtFOpenOnSundays1 = TestCase (assertBool "should be closed on a sunday" (isClosed nineToFiveOnWeekDays sundayAtNoon))

 
tests :: Test
tests = TestList [
    TestLabel "dsAlwaysOpenOnMondays" dsAlwaysOpenOnMondays,
    TestLabel "dsAlwaysClosedOnMondays" dsAlwaysClosedOnMondays,
    TestLabel "dsOpenDuringWorkingHours1" dsOpenDuringWorkingHours1,
    TestLabel "dsOpenDuringWorkingHours2" dsOpenDuringWorkingHours2,
    TestLabel "dsOpenDuringWorkingHours3" dsOpenDuringWorkingHours3,
    TestLabel "dsOpenDuringWorkingHours4" dsOpenDuringWorkingHours4,
    TestLabel "dsClosedOutsideWorkingHours1" dsClosedOutsideWorkingHours1,
    TestLabel "dsClosedOutsideWorkingHours2" dsClosedOutsideWorkingHours2,
    TestLabel "dsClosedOutsideWorkingHours3" dsClosedOutsideWorkingHours3,
    TestLabel "dsClosedOutsideWorkingHours4" dsClosedOutsideWorkingHours4,
    TestLabel "dsOpenDuringWorkingHoursLunchBreak1" dsOpenDuringWorkingHoursLunchBreak1,
    TestLabel "dsOpenDuringWorkingHoursLunchBreak2" dsOpenDuringWorkingHoursLunchBreak2,
    TestLabel "dsOpenDuringWorkingHoursLunchBreak3" dsOpenDuringWorkingHoursLunchBreak3,
    TestLabel "dsOpenDuringWorkingHoursLunchBreak4" dsOpenDuringWorkingHoursLunchBreak4,
    TestLabel "dsClosedOutsideWorkingHoursLunchBreak1" dsClosedOutsideWorkingHoursLunchBreak1,
    TestLabel "dsClosedOutsideWorkingHoursLunchBreak2" dsClosedOutsideWorkingHoursLunchBreak2,
    TestLabel "dsClosedOutsideWorkingHoursLunchBreak3" dsClosedOutsideWorkingHoursLunchBreak3,
    TestLabel "dsClosedOutsideWorkingHoursLunchBreak4" dsClosedOutsideWorkingHoursLunchBreak4,

    TestLabel "wsOpenOnMondays1" wsOpenOnMondays1,
    TestLabel "wsOpenOnMondays2" wsOpenOnMondays2,
    TestLabel "wsOpenOnTuesdays1" wsOpenOnTuesdays1,
    TestLabel "wsOpenOnTuesdays2" wsOpenOnTuesdays2,
    TestLabel "wsOpenOnWednesdays1" wsOpenOnWednesdays1,
    TestLabel "wsOpenOnWednesdays2" wsOpenOnWednesdays2,
    TestLabel "wsOpenOnThursday1" wsOpenOnThursday1,
    TestLabel "wsOpenOnThursday2" wsOpenOnThursday2,
    TestLabel "wsOpenOnFridays1" wsOpenOnFridays1,
    TestLabel "wsOpenOnFridays2" wsOpenOnFridays2,
    TestLabel "wsOpenOnSaturdays1" wsOpenOnSaturdays1,
    TestLabel "wsOpenOnSundays1" wsOpenOnSundays1,

    TestLabel "wsNtFOpenOnMondays1" wsNtFOpenOnMondays1,
    TestLabel "wsNtFOpenOnMondays2" wsNtFOpenOnMondays2,
    TestLabel "wsNtFOpenOnTuesdays1" wsNtFOpenOnTuesdays1,
    TestLabel "wsNtFOpenOnTuesdays2" wsNtFOpenOnTuesdays2,
    TestLabel "wsNtFOpenOnWednesdays1" wsNtFOpenOnWednesdays1,
    TestLabel "wsNtFOpenOnWednesdays2" wsNtFOpenOnWednesdays2,
    TestLabel "wsNtFOpenOnThursday1" wsNtFOpenOnThursday1,
    TestLabel "wsNtFOpenOnThursday2" wsNtFOpenOnThursday2,
    TestLabel "wsNtFOpenOnFridays1" wsNtFOpenOnFridays1,
    TestLabel "wsNtFOpenOnFridays2" wsNtFOpenOnFridays2,
    TestLabel "wsNtFOpenOnSaturdays1" wsNtFOpenOnSaturdays1,
    TestLabel "wsNtFOpenOnSundays1" wsNtFOpenOnSundays1
    ]
 
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
 