import ParseLib.Abstract
import System.Environment
import System.IO
import Prelude hiding ((<*),(*>), (++), sequence)
import Data.List hiding (find)
import Data.Maybe
import qualified Data.Time as DT
-- Starting Framework

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year { unYear :: Int }  deriving (Eq, Ord)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day { unDay :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour { unHour :: Int } deriving (Eq, Ord)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord)


-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = mainDateTime

mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show

-- We changed putStrLn to putStr so that it supports added breakpoints (\n)
mainCalendar :: IO ()
mainCalendar = do
    file:_ <- getArgs
    res <- readCalendar file
    putStr $ maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11)) res

-- Debug function to see whether the calendar parsing and printing works
testCalendar :: FilePath -> IO ()
testCalendar p = do
                res <- readCalendar p
                putStr $ maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11)) res

-- Exercise 1
-- Parse a DateTime 
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* parseSep <*> parseTime <*> parseUTC

-- Parse a Date
parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

-- Parse a Year
parseYear :: Parser Char Year
parseYear = toYear <$> digit <*> digit <*> digit <*> digit
          where toYear a b c d = Year $ toInt $ charList a b c d
                charList a b c d = a : b : c : d : []

-- Parse a Month                
parseMonth :: Parser Char Month
parseMonth = toMonth <$> digit <*> digit
           where toMonth a b = Month $ toInt $ tdtd a b

-- Parse a Day           
parseDay :: Parser Char Day
parseDay = toDay <$> digit <*> digit
         where toDay a b = Day $ toInt $ tdtd a b

-- Parse a Time         
parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

-- Parse an Hour         
parseHour :: Parser Char Hour
parseHour = toHour <$> digit <*> digit
          where toHour a b = Hour $ toInt $ tdtd a b

-- Parse a Minute         
parseMinute :: Parser Char Minute
parseMinute = toMinute <$> digit <*> digit
            where toMinute a b = Minute $ toInt $ tdtd a b

-- Parse a Second         
parseSecond :: Parser Char Second
parseSecond = toSecond <$> digit <*> digit
            where toSecond a b = Second $ toInt $ tdtd a b

-- Parse a seperator           
parseSep :: Parser Char Char
parseSep = satisfy (=='T')

-- Check if the input has a Z
zToBool :: Char -> Bool
zToBool 'Z' = True
zToBool _   = False

-- Parse the seperator  
parseUTC :: Parser Char Bool
parseUTC = zToBool <$> option (satisfy (=='Z')) 'F'

-- Combine two chars to a list
tdtd :: Char -> Char -> [Char]
tdtd a b = a : b : []

-- String to Int
toInt :: String -> Int
toInt xs = read xs

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p s = case prs of
    ((_,[]):_) -> Just $ fst $ head prs
    _      -> Nothing
    where prs = parse p s


-- Exercise 3
printDateTime :: DateTime -> String
-- year month day datesep hour minute second timeutc
printDateTime dt = y ++ m ++ da ++ datesep ++ h ++ mi ++ s ++ timeutc
            where d = date dt -- Date
                  t = time dt -- Time
                  y = show $ unYear $ year d -- Year
                  m = show $ unMonth $ month d -- Month
                  da = show $ unDay $ day d -- Day
                  datesep = "T"
                  h = show $ unHour $ hour t -- Hour
                  mi = show $ unMinute $ minute t -- Minute
                  s = show $ unSecond $ second t -- Seconds
                  timeutc = bToS $ utc dt
                  bToS True  = "Z"
                  bToS False = ""

-- Exercise 4
parsePrint :: String -> Maybe String
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime dt = checkDate (date dt) && checkTime (time dt)
                where checkDate d = True && checkMonth (unMonth (month d)) && checkDay (unDay (day d))
                      checkTime t = checkHour (unHour (hour t)) && checkMinute (unMinute (minute t)) && checkSecond (unSecond (second t))
                      checkMonth m = m >= 1 && m <= 12
                      -- See whether the day is correct by checking the month and year, if needed
                      checkDay day | unMonth (month (date dt)) `elem` [1, 3, 5, 7, 8, 10, 12] = day >= 1 && day <= 31
                                   | unMonth (month (date dt)) `elem` [4, 6, 9, 11]           = day >= 1 && day <= 30
                                   | unMonth (month (date dt)) == 2                           = case (unYear (year (date dt))) `mod` 4 of
                                      0 -> day >= 1 && day <= 29
                                      _ -> day >= 1 && day <= 28
                                   | otherwise = False
                      checkHour h = h >= 0 && h <= 23
                      checkMinute m = m >= 0 && m <= 59
                      checkSecond s = s >= 0 && s <= 59

-- Exercise 6
data Calendar = Calendar BeginEnd [Calprop] [Event] BeginEnd
    deriving (Eq, Ord, Show)

type Text = String

data BeginEnd = Begin Text | End Text 
    deriving (Eq, Ord)
instance Show BeginEnd where
        show (Begin t) = "BEGIN:" ++ t ++ crlf
        show (End t)   = "END:" ++ t ++ crlf
   
data Calprop = Prodid {unProdid :: Text} | Version {unVersion :: Text}
    deriving (Eq, Ord)
instance Show Calprop where
    show (Prodid t)  = "PRODID:" ++ t ++ crlf
    show (Version t) = "VERSION:" ++ t ++ crlf

data Event = Event BeginEnd [Property] BeginEnd
    deriving (Eq, Ord, Show)

data Property   = Dtstamp     { unStamp :: DateTime}
                | Uid         { unUid :: Text}
                | Dtstart     { unStart :: DateTime }
                | Dtend       { unEnd :: DateTime }
                | Description { unDesc :: Text }
                | Summary     { unSum :: Text }
                | Location    { unLoc :: Text}
    deriving (Eq, Ord)
    
instance Show Property where
    show (Dtstamp dt)    = "DTSTAMP:" ++ show dt
    show (Uid t)         = "UID:" ++ t
    show (Dtstart dt)    = "DTSTART:" ++ show dt
    show (Dtend dt)      = "DTEND:" ++ show dt
    show (Description t) = "DESCRIPTION:" ++ t
    show (Summary t)     = "SUMMARY:" ++ t
    show (Location t)    = "LOCATION:" ++ t

crlf :: String
crlf = "\r\n"

enter :: String
enter = "\n"

-- Exercise 7
data Token = Tbegincalendar Text | Tprodid Text | Tversion Text | Tbeginevent Text | Tdtstamp DateTime | Tuid Text | Tdtstart DateTime | Tdtend DateTime | Tdescription Text | Tsummary Text | Tlocation Text | TendEvent Text | TendCalendar Text
     deriving (Eq, Ord, Show)
type Prefix = Text
data Suffix = SText Text | SDT DateTime
     deriving (Eq, Ord, Show)
  
---------------------------LEXING-------------------------------
-- See if the input parses to any token
scanCalendar :: Parser Char [Token]
scanCalendar = greedy1 anyToken <* eof
             where anyToken = tbegincalendar <|> tprodid <|> tversion <|> tendcalendar <|> tbeginevent <|> tdtstamp <|> tuid <|> tdtstart <|> tdtend <|> tdescription <|> tsummary <|> tlocation <|> tendevent

-- Function that parses until it hits an crlf
anyChar :: Parser Char String
anyChar = ((const []) <$> token crlf) <<|>  (:) <$> anySymbol <*> anyChar

-- Always has to parse the VCALENDAR value
tbegincalendar :: Parser Char Token
tbegincalendar = Tbegincalendar <$> (token "BEGIN:" *> token ("VCALENDAR") <* token crlf)

tprodid :: Parser Char Token
tprodid = Tprodid <$> (token "PRODID:" *> anyChar)

tversion :: Parser Char Token
tversion = Tversion <$> (token "VERSION:" *> anyChar)

-- Always has to parse the VCALENDAR value
tendcalendar :: Parser Char Token
tendcalendar = TendCalendar <$> (token "END:" *> token "VCALENDAR" <* token crlf)

-- Always has to parse the VEVENT value
tbeginevent :: Parser Char Token
tbeginevent = Tbeginevent <$> (token "BEGIN:" *> token "VEVENT" <* token crlf)

tdtstamp :: Parser Char Token
tdtstamp = Tdtstamp <$> (token "DTSTAMP:" *> parseDateTime <* token crlf)

tuid :: Parser Char Token
tuid = Tuid <$> (token "UID:" *> anyChar)

tdtstart :: Parser Char Token
tdtstart = Tdtstart <$> (token "DTSTART:" *> parseDateTime <* token crlf)

tdtend :: Parser Char Token
tdtend = Tdtend <$> (token "DTEND:" *> parseDateTime <* token crlf)

tdescription :: Parser Char Token
tdescription = Tdescription <$> (token "DESCRIPTION:" *> anyChar)

tsummary :: Parser Char Token
tsummary = Tsummary <$> (token "SUMMARY:" *> anyChar)

tlocation :: Parser Char Token
tlocation = Tlocation <$> (token "LOCATION:" *> anyChar)

-- Always has to parse the VCALENDAR value
tendevent :: Parser Char Token
tendevent = TendEvent <$> (token "END:" *> token "VEVENT" <* token crlf)

---------------------------PARSING------------------------------
parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$> beginCalendar <*> checkProps <*> many parseEvent <*> endCalendar <* eof
              where checkProps = choice $ map sequence calprops

parseEvent :: Parser Token Event
parseEvent = Event <$> beginEvent <*> parseProps <*> endEvent

-- Run all of the possible parsers, and pick the one that succeeds
parseProps :: Parser Token [Property]
parseProps = choice $ map sequence allSequences

calprops :: [[Parser Token Calprop]]
calprops = [[prodid, version],[version, prodid]]

-- Generates every possible sequence of properties
allSequences :: [[Parser Token Property]]
allSequences = foldr (++) [] $ map permutations $ map (++ perms) subs
             where perms = [dtstamp,uid,dtstart,dtend]
                   subs  = subsequences [description, summary, location]

-- Cover your eyes please, a lot of duplicated code coming right up
beginCalendar :: Parser Token BeginEnd
beginCalendar = fromBegincalendar <$> satisfy isBegincalendar
isBegincalendar :: Token -> Bool
isBegincalendar (Tbegincalendar _) = True
isBegincalendar _ = False
fromBegincalendar :: Token -> BeginEnd
fromBegincalendar (Tbegincalendar x) = Begin x
fromBegincalendar _ = error "Hoort hier niet te komen"

prodid :: Parser Token Calprop
prodid = fromProdid <$> satisfy isProdid
isProdid :: Token -> Bool
isProdid (Tprodid _) = True
isProdid _ = False
fromProdid :: Token -> Calprop
fromProdid (Tprodid x) = Prodid x
fromProdid _ = error "Hoort hier niet te komen"

version :: Parser Token Calprop
version = fromVersion <$> satisfy isVersion
isVersion :: Token -> Bool
isVersion (Tversion _) = True
isVersion _ = False
fromVersion :: Token -> Calprop
fromVersion (Tversion x) = Version x
fromVersion _ = error "Hoort hier niet te komen"

endCalendar :: Parser Token BeginEnd
endCalendar = fromEndcalendar <$> satisfy isEndcalendar
isEndcalendar :: Token -> Bool
isEndcalendar (TendCalendar _) = True
isEndcalendar _ = False
fromEndcalendar :: Token -> BeginEnd
fromEndcalendar (TendCalendar x) = End x
fromEndcalendar _ = error "Hoort hier niet te komen"

beginEvent :: Parser Token BeginEnd
beginEvent = fromBeginevent <$> satisfy isBeginevent
isBeginevent :: Token -> Bool
isBeginevent (Tbeginevent _) = True
isBeginevent _ = False
fromBeginevent :: Token -> BeginEnd
fromBeginevent (Tbeginevent x) = Begin x
fromBeginevent _ = error "Hoort hier niet te komen"

dtstamp :: Parser Token Property
dtstamp = fromDtstamp <$> satisfy isDtstamp
isDtstamp :: Token -> Bool
isDtstamp (Tdtstamp _) = True
isDtstamp _ = False
fromDtstamp :: Token -> Property
fromDtstamp (Tdtstamp x) = Dtstamp x
fromDtstamp _ = error "Hoort hier niet te komen"

uid :: Parser Token Property
uid = fromUid <$> satisfy isUid
isUid :: Token -> Bool
isUid (Tuid _) = True
isUid _ = False
fromUid :: Token -> Property
fromUid (Tuid x) = Uid x
fromUid _ = error "Hoort hier niet te komen"

dtstart :: Parser Token Property
dtstart = fromDtstart <$> satisfy isDtstart
isDtstart :: Token -> Bool
isDtstart (Tdtstart _) = True
isDtstart _ = False
fromDtstart :: Token -> Property
fromDtstart (Tdtstart x) = Dtstart x
fromDtstart _ = error "Hoort hier niet te komen"

dtend :: Parser Token Property
dtend = fromDtend <$> satisfy isDtend
isDtend :: Token -> Bool
isDtend (Tdtend _) = True
isDtend _ = False
fromDtend :: Token -> Property
fromDtend (Tdtend x) = Dtend x
fromDtend _ = error "Hoort hier niet te komen"

description :: Parser Token Property
description = fromDecription <$> satisfy isDescription
isDescription :: Token -> Bool
isDescription (Tdescription _) = True
isDescription _ = False
fromDecription :: Token -> Property
fromDecription (Tdescription x) = Description x
fromDecription _ = error "Hoort hier niet te komen"

summary :: Parser Token Property
summary = fromSummary <$> satisfy isSummary
isSummary :: Token -> Bool
isSummary (Tsummary _) = True
isSummary _ = False
fromSummary :: Token -> Property
fromSummary (Tsummary x) = Summary x
fromSummary _ = error "Hoort hier niet te komen"

location :: Parser Token Property
location = fromLocation <$> satisfy isLocation
isLocation :: Token -> Bool
isLocation (Tlocation _) = True
isLocation _ = False
fromLocation :: Token -> Property
fromLocation (Tlocation x) = Location x
fromLocation _ = error "Hoort hier niet te komen"

endEvent :: Parser Token BeginEnd
endEvent = fromEndevent <$> satisfy isEndevent
isEndevent :: Token -> Bool
isEndevent (TendEvent _) = True
isEndevent _ = False
fromEndevent :: Token -> BeginEnd
fromEndevent (TendEvent x) = End x
fromEndevent _ = error "Hoort hier niet te komen"

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do
                    handle <- openFile path ReadWriteMode
                    _ <- hSetNewlineMode handle noNewlineTranslation
                    content <- hGetContents handle
                    return $ recognizeCalendar content

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that 😂👌🅱
printCalendar :: Calendar -> String
printCalendar (Calendar begin props events end) = show begin ++ foldr (++) [] (map show props) ++ foldr (++) [] (map printEvent events) ++ show end

printEvent :: Event -> String
printEvent (Event begin props end) = show begin ++ foldr (++) [] (map printProp props) ++ show end

printProp :: Property -> String
printProp p = show p ++ crlf

-- Exercise 10

-- Checks how many events are in the calendar
countEvents :: Calendar -> Int
countEvents (Calendar _ _ events _) = length events

-- Looks for events that are happening at a given date and time in a calendar
findEvents :: DateTime -> Calendar -> [Event]
findEvents time (Calendar _ _ events _) = filter (find time) events

-- Chekcs if an event happens during a certain time
find :: DateTime -> Event -> Bool
find time (Event _ props _) | (time >= start && time < end) = True
                            | otherwise                     = False
                            where start = getStartTime props
                                  end   = getEndTime props

-- Get the start time from a list of properties                                
getStartTime :: [Property] -> DateTime
getStartTime [] = error "Invalid event"
getStartTime ((Dtstart time):xs) = time
getStartTime (_:xs) = getStartTime xs

-- Get the end time from a list of properties  
getEndTime :: [Property] -> DateTime
getEndTime [] = error "Invalid event"
getEndTime ((Dtend time):xs) = time
getEndTime (_:xs) = getEndTime xs

-- Check if events overlap in a calendar
checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ _ events _)  | length overlapping > 0 = True
                                          | otherwise              = False
                                        where combinations = [(i,j)| i <- events, j <- events, i /= j]
                                              overlapping  = filter doesOverlap combinations

 -- Checks if two events overlap                                             
doesOverlap :: (Event, Event) -> Bool
doesOverlap ((Event _ props1 _), (Event _ props2 _)) | ( (beg1 <= end2) && end1 >= beg2) = True
                                                     | otherwise                         = False
                                                    where beg1 = getStartTime props1
                                                          beg2 = getStartTime props2
                                                          end1 = getEndTime props1
                                                          end2 = getEndTime props2 

-- Total amount of minutes spend on events with a given summary                                                          
timeSpent :: String -> Calendar -> Int
timeSpent summary (Calendar _ _ events _) = sum [eventTime x | x <- matchingEvents] -- TODO: dit fixen met min begin en max eindtijd
                                            where matchingEvents = filterEventsThatMatch (zipEventsWithBools events  (propsMatchSumm summary events))
                          
-- Filter the events that do match the summary                        
filterEventsThatMatch :: [(Event, Bool)] -> [Event]
filterEventsThatMatch [] = []
filterEventsThatMatch ((x, True):xs) = x : filterEventsThatMatch xs
filterEventsThatMatch ((x, False):xs) = filterEventsThatMatch xs

-- Zips the events with a bool if the event has the summary
zipEventsWithBools :: [Event] -> [Bool] -> [(Event, Bool)]
zipEventsWithBools events bools = zip events bools

-- return a list of Bools that tell whether or not the event has the given summary
propsMatchSumm :: String -> [Event] -> [Bool]
propsMatchSumm summary events = map (matchSummary summary) (getProps events)

-- Get all the properties from a list of events
getProps :: [Event] -> [[Property]]
getProps [] = []
getProps ((Event _ props _):xs) = props : getProps xs

-- Check if a list of properties contains the given summary
matchSummary :: String -> [Property] -> Bool
matchSummary summary props = (getSummary props) == summary

-- Get the summary from a list of properties
getSummary :: [Property] -> String
getSummary [] = error "Invalid event"
getSummary ((Summary summ):xs) = summ
getSummary (_:xs) = getSummary xs

-- Calculate the duration of an event in minutes 
eventTime :: Event -> Int
eventTime (Event _ props _) = timeDifference beg end
                        where beg   = getStartTime props
                              end   = getEndTime props

-- Calculate the difference in minutes between the start and endtime of an event
timeDifference :: DateTime -> DateTime -> Int
timeDifference t1 t2 = diffInMins + 60 * (hour2 - hour1) + (min2 - min1)
                    where dtTup1     = dayTimeConversion t1
                          dtTup2     = dayTimeConversion t2 
                          hour1      = unHour $ hour $ time $ t1
                          hour2      = unHour $ hour $ time $ t2
                          min1       = unMinute $ minute $ time $ t1
                          min2       = unMinute $ minute $ time $ t2
                          diff       = fromIntegral (DT.diffDays dtTup2 dtTup1)
                          diffInMins = diff * 1440
                                   
-- Convert a DateTime to a Day from the time library                     
dayTimeConversion :: DateTime -> DT.Day         
dayTimeConversion dt = newDay
                    where yr  = unYear $ year $ date $ dt
                          mnt = unMonth $ month $ date $ dt
                          dy   = unDay $ day $ date $ dt
                          newDay = DT.fromGregorian (toInteger yr) mnt dy
                                
-- Exercise 11
-- Draw the calendar in the console
ppMonth :: Year -> Month -> Calendar -> String
ppMonth y m (Calendar _ _ events _) = getWeeks daysCount validEvents
                                    where daysCount      = getAmountOfDays y m
                                          validEvents    = sortByDay (filterEvents y m events) daysCount [[]]

-- Print all the weeks                                          
getWeeks :: Int -> [[Event]] -> String
getWeeks 28 events = div ++ getWeek (1,7) events ++ div ++ getWeek (8,14) events ++ div ++ getWeek (15,21) events ++ div ++ getWeek (22,28) events ++ div
                   where div = divider 7
getWeeks d events  = div ++ getWeek (1,7) events ++ div ++ getWeek (8,14) events ++ div ++ getWeek (15,21) events ++ div ++ getWeek (22,28) events ++ div ++ getWeek (29, d) events ++ divider (d - 28)
                   where div = divider 7

-- Get all the lines of a week                   
getWeek :: (Int, Int) -> [[Event]] -> String
getWeek (low, high) events = [y | x <- [0 .. rowHeight], y <- (foldr (++) [] (map (\z -> z !! x) getDays)) ++ "|" ++ enter]
                           where rowHeight = maximum $ map length correctEvents
                                 getDays = [getDay y rowHeight (events !! y) | y <- [low .. high]]
                                 correctEvents = drop (low - 1) $ take high events

-- Get a day with all events
getDay :: Int -> Int -> [Event] -> [String]
getDay day height events = printDayNumber : printEvents
                         where printDayNumber = case day < 10 of
                                                True -> "| " ++ show day ++ replicate 13 ' '
                                                _    -> "| " ++ show day ++ replicate 12 ' '
                               printEvents = map printEvent events ++ replicate (height - length events) emptyLine
                               printEvent e = "| " ++ getStart e ++ " - " ++ getEnd e ++ " "
                               getStart = (\x -> zeros (unHour(hour (time (getStartTime (z x))))) ++ "-" ++ zeros (unMinute(minute (time (getStartTime (z x))))))
                               getEnd = (\x -> zeros (unHour(hour (time (getEndTime (z x))))) ++ "-" ++ zeros (unMinute(minute (time (getEndTime (z x))))))
                               emptyLine = "|" ++ replicate 15 ' '
                               z = getEventProps
                               zeros g = case g < 10 of
                                         True -> "0" ++ show g
                                         _    -> show g

-- Get all the properties of an event
getEventProps :: Event -> [Property]
getEventProps (Event _ props _) = props

-- Get all the events of a rowindex
getRowEvents :: Int -> [Event] -> [Event]
getRowEvents row events = filter (\x -> func x) events 
                        where func y = getDayFromEvent y > (row * 7 - 7) && getDayFromEvent y <= (row * 7)

-- Get the day index from an event
getDayFromEvent :: Event -> Int
getDayFromEvent (Event _ props _) = unDay $ day $ date $ getStartTime props

-- Get the amount of days in a month
getAmountOfDays :: Year -> Month -> Int
getAmountOfDays y (Month 2)     | (unYear y) `mod` 4 == 0 = 29
                                | otherwise      = 28                        
getAmountOfDays _ month         | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
                                | otherwise                                                            = 30
                                where m = unMonth month

-- Get all the events in a given month and year                                
filterEvents :: Year -> Month -> [Event] -> [Event]
filterEvents y m events = filter (\x -> years x == (unYear y) && (months x == (unMonth m))) events
                      where years (Event _ p _) = unYear $ year $ date $ getStartTime p
                            months (Event _ p _)= unMonth $ month $ date $ getStartTime p

-- Sort all the events in lists of events per day                            
sortByDay :: [Event] -> Int -> [[Event]] -> [[Event]]
sortByDay events 0 acc          = acc
sortByDay events currentDay acc = sortByDay events (currentDay - 1) $ checkIfDay : acc
                                where checkIfDay = filter (\x -> getDayFromEvent x == currentDay) events

-- Print a divider                                
divider :: Int -> String
divider a = "+" ++ (concat $ replicate a hyphensplus) ++ enter
         where hyphensplus = replicate 15 '-' ++ "+"