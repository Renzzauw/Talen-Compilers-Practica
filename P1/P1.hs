import ParseLib.Abstract
import System.Environment
import System.IO
import Prelude hiding ((<*),(*>), (<>), sequence)
import Data.List hiding (find)
import Text.PrettyPrint

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

mainCalendar :: IO ()
mainCalendar = do
    file:_ <- getArgs
    res <- readCalendar file
    putStrLn $ maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11)) res

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* parseSep <*> parseTime <*> parseUTC

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseYear :: Parser Char Year
parseYear = toYear <$> digit <*> digit <*> digit <*> digit
          where toYear a b c d = Year $ toInt $ charList a b c d
                charList a b c d = a : b : c : d : []

parseMonth :: Parser Char Month
parseMonth = toMonth <$> digit <*> digit
           where toMonth a b = Month $ toInt $ tdtd a b

parseDay :: Parser Char Day
parseDay = toDay <$> digit <*> digit
         where toDay a b = Day $ toInt $ tdtd a b

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseHour :: Parser Char Hour
parseHour = toHour <$> digit <*> digit
          where toHour a b = Hour $ toInt $ tdtd a b

parseMinute :: Parser Char Minute
parseMinute = toMinute <$> digit <*> digit
            where toMinute a b = Minute $ toInt $ tdtd a b


parseSecond :: Parser Char Second
parseSecond = toSecond <$> digit <*> digit
            where toSecond a b = Second $ toInt $ tdtd a b

parseSep :: Parser Char Char
parseSep = satisfy (=='T')

zToBool :: Char -> Bool
zToBool 'Z' = True
zToBool _   = False

parseUTC :: Parser Char Bool
parseUTC = zToBool <$> option (satisfy (=='Z')) 'F'

-- Combine two chars to a list
tdtd :: Char -> Char -> [Char]
tdtd a b = a : b : []

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
    show (Summary t)     = "SUMMARY" ++ t
    show (Location t)    = "LOCATION" ++ t

crlf :: String
crlf = "\\r\\n"


-- Exercise 7
data Token = Tbegincalendar Text | Tprodid Text | Tversion Text | Tbeginevent Text | Tdtstamp DateTime | Tuid Text | Tdtstart DateTime | Tdtend DateTime | Tdescription Text | Tsummary Text | Tlocation Text | TendEvent Text | TendCalendar Text
     deriving (Eq, Ord, Show)
type Prefix = Text
data Suffix = SText Text | SDT DateTime
     deriving (Eq, Ord, Show)
  
---------------------------LEXING-------------------------------
scanCalendar :: Parser Char [Token]
scanCalendar = greedy1 getCalTokens <* eof
    where getCalTokens = tbegincalendar <|> tprodid <|> tversion <|> getEventTokens <|> tendcalendar
          getEventTokens = tbeginevent <|> tdtstamp <|> tuid <|> tdtstart <|> tdtend <|> tdescription <|> tsummary <|> tlocation <|> tendevent

-- Always has to parse the VCALENDAR value
tbegincalendar :: Parser Char Token
tbegincalendar = Tbegincalendar <$> (token "BEGIN:" *> token "VCALENDAR")

tprodid :: Parser Char Token
tprodid = Tprodid <$> (token "PRODID:" *> identifier)

tversion :: Parser Char Token
tversion = Tversion <$> (token "VERSION:" *> identifier)

-- Always has to parse the VCALENDAR value
tendcalendar :: Parser Char Token
tendcalendar = TendCalendar <$> (token "END:" *> token "VCALENDAR")

-- Always has to parse the VEVENT value
tbeginevent :: Parser Char Token
tbeginevent = Tbeginevent <$> (token "BEGIN:" *> token "VEVENT")

tdtstamp :: Parser Char Token
tdtstamp = Tdtstamp <$> (token "DTSTAMP:" *> parseDateTime)

tuid :: Parser Char Token
tuid = Tuid <$> (token "UID:" *> identifier)

tdtstart :: Parser Char Token
tdtstart = Tdtstart <$> (token "DTSTART:" *> parseDateTime)

tdtend :: Parser Char Token
tdtend = Tdtend <$> (token "DTEND:" *> parseDateTime)

tdescription :: Parser Char Token
tdescription = Tdescription <$> (token "DESCRIPTION:" *> identifier)

tsummary :: Parser Char Token
tsummary = Tsummary <$> (token "SUMMARY:" *> identifier)

tlocation :: Parser Char Token
tlocation = Tlocation <$> (token "LOCATION:" *> identifier)

-- Always has to parse the VCALENDAR value
tendevent :: Parser Char Token
tendevent = TendEvent <$> (token "END:" *> token "VEVENT")

---------------------------PARSING------------------------------
parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$> beginCalendar <*> checkProps <*> many parseEvent <*> endCalendar <* eof
              where checkProps = choice $ map sequence calprops

parseEvent :: Parser Token Event
parseEvent = Event <$> beginEvent <*> parseProps <*> endEvent

parseProps :: Parser Token [Property]
parseProps = choice $ map sequence withoutopt

calprops :: [[Parser Token Calprop]]
calprops = [[prodid, version],[version, prodid]]

withoutopt :: [[Parser Token Property]]
withoutopt = foldr (++) [] $ map permutations $ map (++ perms) subs
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
endEvent = fromEndcalendar <$> satisfy isEndcalendar
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
                    handle <- openFile path ReadMode
                    _ <- hSetNewlineMode handle noNewlineTranslation
                    content <- hGetContents handle
                    return $ recognizeCalendar content


-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar (Calendar begin props events end) = show begin ++ foldr (++) [] (map show props) ++ foldr (++) [] (map printEvent events) ++ show end

printEvent :: Event -> String
printEvent (Event begin props end) = show begin ++ foldr (++) [] (map printProp props) ++ show end

printProp :: Property -> String
printProp p = show p ++ crlf

-- Exercise 10
countEvents :: Calendar -> Int
countEvents (Calendar _ _ events _) = length events

findEvents :: DateTime -> Calendar -> [Event]
findEvents time (Calendar _ _ events _) = filter (find time) events

find :: DateTime -> Event -> Bool
find time (Event _ props _) | (time >= start && time <= end) = True
                            | otherwise                      = False
                            where start = getStartTime props
                                  end   = getEndTime props

getStartTime :: [Property] -> DateTime
getStartTime [] = error "Invalid event"
getStartTime ((Dtstart time):xs) = time
getStartTime (_:xs) = getStartTime xs

getEndTime :: [Property] -> DateTime
getEndTime [] = error "Invalid event"
getEndTime ((Dtend time):xs) = time
getEndTime (_:xs) = getStartTime xs

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ _ events _)  | length overlapping > 0 = True
                                            | otherwise              = False
                                           where combinations = [(i,j)| i <- events, j <- events, i /= j]
                                                 overlapping = filter doesOverlap combinations
-- TODO: isje verwijderen?
doesOverlap :: (Event, Event) -> Bool
doesOverlap ((Event _ props1 _), (Event _ props2 _)) | ( (beg1 <= end2) && end1 >= beg2) = True
                                                     | otherwise                         = False
                                                    where beg1 = getStartTime props1
                                                          beg2 = getStartTime props2
                                                          end1 = getEndTime props1
                                                          end2 = getEndTime props2 

timeSpent :: String -> Calendar -> Int
timeSpent summary (Calendar _ _ events _) = sum [eventTime x | x <- matchingEvents]
                                            where matchingEvents = filterEventsThatMatch (zipEventsWithBools events  (propsMatchSumm summary events))

filterEventsThatMatch :: [(Event, Bool)] -> [Event]
filterEventsThatMatch [] = []
filterEventsThatMatch [(x, True), xs] = x : filterEventsThatMatch [xs]
filterEventsThatMatch [(x, False), xs] = filterEventsThatMatch [xs]

zipEventsWithBools :: [Event] -> [Bool] -> [(Event, Bool)]
zipEventsWithBools events bools = zip events bools

propsMatchSumm :: String -> [Event] -> [Bool]
propsMatchSumm summary events = map (matchSummary summary) (getProps events)

getProps :: [Event] -> [[Property]]
getProps [] = []
getProps ((Event _ props _):xs) = props : getProps xs

matchSummary :: String -> [Property] -> Bool
matchSummary summary props = (getSummary props) == summary

getSummary :: [Property] -> String
getSummary [] = error "Invalid event"
getSummary ((Summary summ):xs) = summ
getSummary (_:xs) = getSummary xs

eventTime :: Event -> Int
eventTime (Event _ props _) = timeDifference beg end
                        where beg   = time (getStartTime props)
                              end   = time (getEndTime props)

-- Not sure of deze klopt qua minuten aftrekken van elkaar...
timeDifference :: Time -> Time -> Int
timeDifference t1 t2 = hours * 60 + mins
                    where hours = unHour(hour t2) - unHour(hour t1)
                          mins  = unMinute(minute t2) - unMinute(minute t2)


-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth y m (Calendar _ _ events _) = render pcalendar
                                    where daysCount = getAmountOfDays y m
                                          pcalendar = foldr (++crlf) printDayLine getWeeks
                                          events    = filterEvents y m events

                                          --getWeeks = 

--getWeeks :: Int -> 
 --                                           case daysCount of 
  --                                                   28 -> --doe 4x week maken
   --                                                   _  -> --doe 5x week maken

printDaynumbers :: Int -> Int -> Doc
printDaynumbers row maxDays = foldr (<>) Text.PrettyPrint.empty [printDayLine x | x <- [(1 + 7*row) .. (7 + 7*row)], x <= maxDays]

getRowEvents :: Int -> [Event] -> [Event]
getRowEvents row events = filter (\x -> func x) events 
                        where func y = getDay y > (row * 7 - 7) && getDay y <= (row * 7)

getDay :: Event -> Int
getDay (Event _ props _) = unDay $ day $ date $ findstamp props

findstamp :: [Property] -> DateTime
findstamp [] = error "Dtstamp should exist"
findstamp ((Dtstamp x): xs) = x
findstamp (_:xs)            = findstamp xs



getAmountOfDays :: Year -> Month -> Int
getAmountOfDays y 2 | y `mod` 4 == 0 = 29
                    | otherwise      = 28                        
getAmountOfDays _ m | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
                    | otherwise                                                            = 30

filterEvents :: Year -> Month -> [Event] -> [Event]
filterEvents y m events = filter (\x -> years x == (unYear y) && (months x == (unMonth m))) events
                      where years (Event _ p _) = unYear $ year $ date $ findstamp p
                            months (Event _ p _)= unMonth $ month $ date $ findstamp p

sortByDay :: [Event] -> Int -> [[Event]]
sortByDay events dayCount = [x | x <- events, y <- [1 .. dayCount], getDay x == y]

-- 7 blokken breed
-- 5 blokken hoog 
-- dagnummer per blok
-- tijden per blok
-- *** Printing functions *** --


printDayLine :: Int -> String
printDayLine 0 = verticalLine <> printSpaces 15 -- Empty calendar space, default value is 0
printDayLine num = verticalLine <> int num <> printSpaces 14

-- Print the event in the following format: "|20:00-21:00    "
printEventLine :: Event -> Doc
printEventLine (Event _ props _) = verticalLine <> text startTime <> hyphen <> text endTime <> printSpaces 4
                            where startTime = printDateTime (getStartTime props)
                                  endTime = printDateTime (getEndTime props)

-- Print an "empty" line in a calendar day                                  
printEventSpace :: Doc
printEventSpace = verticalLine <> printSpaces 15

printHorizontalDivider :: Doc
printHorizontalDivider = hyphens <> plus <> hyphens <> plus <> hyphens <> plus <> 
                         hyphens <> plus <> hyphens <> plus <> hyphens <> plus <> hyphens
                     where hyphens = printHyphens 15

printSpaces :: Int -> Doc
printSpaces count = text (concat $ replicate count " ")

printHyphens :: Int -> Doc
printHyphens count = text (concat $ replicate count "-")

verticalLine :: Doc 
verticalLine = text "|"

hyphen :: Doc 
hyphen = text "-"

plus :: Doc 
plus = text "+"