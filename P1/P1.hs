import ParseLib.Abstract
import System.Environment
import System.IO
import Prelude hiding ((<*),(*>))
import Data.List.Split

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
data Calendar = Calendar BeginEnd Calprop Calprop [Event] BeginEnd
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
    show (Description t) = "DESCRIPTION:" + t
    show (Summary t)     = "SUMMARY" + t
    show (Location t)    = "LOCATION" + t

crlf :: String
crlf = "\\r\\n"


-- Exercise 7
data Token = Token {unText :: Text}
     deriving (Eq, Ord, Show)
  
scanCalendar :: Parser Char [Token]
scanCalendar = Token <$> greedy1 (identifier <* idParser "\\r\\n")

parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$> begincal <*> calp <*> calp <*> greedy1 event <*> endcal <* eof

event :: Parser Char Event
event = Event <$> beginev <*> greedy1 prop <*> endev

prop :: Parser Char Property
prop = Dtstamp <$> parseDateTime <<|> uid <<|> Dtstart <$> parseDateTime <<|> Dtend <$> parseDateTime <<|> description <<|> summary <<|> location

uid :: Parser Char Property
uid = Uid <$> idParser "UID:"

description :: Parser Char Property
description = Description <$> idParser "DESCRIPTION:"

summary :: Parser Char Property
summary = Summary <$> idParser "SUMMARY:"

location :: Parser Char Property
location = Location <$> idParser "LOCATION:"

-- Always has to parse the VCALENDAR value
begincal :: Parser Char BeginEnd
begincal = Begin <$> doubleParser "BEGIN:" "VCALENDAR"

-- Always has to parse the VEVENT value
beginev :: Parser Char BeginEnd
beginev = Begin <$> doubleParser "BEGIN:" "VEVENT"

-- Always has to parse the VCALENDAR value
endcal :: Parser Char BeginEnd
endcal = End <$> doubleParser "END:" "VCALENDAR"

-- Always has to parse the VCALENDAR value
endev :: Parser Char BeginEnd
endev = End <$> doubleParser "END:" "VEVENT"

-- Has to check for either the Prodid or Calprop
calp :: Parser Char Calprop
calp = prodid <|> version 

prodid :: Parser Char Calprop
prodid = Prodid <$> idParser "PRODID:"

version :: Parser Char Calprop
version = toVersion <$> idParser "VERSION:"
        where toVersion a = Version $ a

-- Parser that removes a given prefix from the string
idParser :: String -> Parser Char String
idParser p = token p *> identifier

-- Same as the idParser, but now the rest has to be a certain string
doubleParser :: String -> String -> Parser Char String
doubleParser p q = token p *> token q

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
printCalendar (Calendar begin prop1 prop2 events end) = show begin ++ show prop1 ++ show prop2 ++ foldr (++) [] (map printEvent events) ++ show end

printEvent :: Event -> String
printEvent (Event begin props end) = show begin ++ foldr (++) [] (map printProp props) ++ show end

printProp :: Property -> String
printProp p = show p ++ crlf

-- Exercise 10
countEvents :: Calendar -> Int
countEvents (Calendar _ _ _ events _) = length events

findEvents :: DateTime -> Calendar -> [Event]
findEvents time (Calendar _ _ _ events _) = filter (find time) events

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
checkOverlapping (Calendar _ _ _ events _)  | length overlapping > 0 = True
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
timeSpent summary (Calendar _ _ _ events _) = --sum [x | x <- eventTime, x moet de summary hebben]
                                            where props = map 

get[Event] -> [[Property]]

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
                    where hours = hour (t2 - t1)
                          mins  = minutes (t2 - t1)


-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined
