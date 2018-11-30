import ParseLib.Abstract
import System.Environment
import Prelude hiding ((<*))
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
data Calendar = Calendar Begin Calprop Calprop Events End
    deriving (Eq, Ord, Show)

type Text = String

data BeginEnd = VCALENDAR | VEVENT 
    deriving (Eq, Ord, Show)

type Begin = BeginEnd
instance Show a => Show (Begin a) where
    show VCALENDAR = "BEGIN:VCALENDAR"
    show VEVENT    = "BEGIN:VEVENT"

type End = BeginEnd
instance Show a => Show (End a) where
    show VCALENDAR = "END:VCALENDAR"
    show VEVENT    = "END:VEVENT"
    
data Calprop = Prodid Text | Version Text
    deriving (Eq, Ord, Show)
instance Show a => Show (Calprop a) where
    show Prodid t  = "PRODID:" + t
    show Version t = "VERSION:" + t

data Events = SingleE Event | MultipleE Event Events   
    deriving (Eq, Ord, Show) 
data Event = Begin Properties End
    deriving (Eq, Ord, Show)

data Properties = SingleP Property | MultipleP Property Properties
    deriving (Eq, Ord, Show)
data Property   = Dtstamp | Uid | Dtstart | Dtend | Description | Summary | Location
    deriving (Eq, Ord, Show)

type Dtstamp     = DateTime 
type Uid         = Text 
type Dtstart     = DateTime 
type Dtend       = DateTime 
type Description = Text 
type Summary     = Text 
type Location    = Text 


-- Exercise 7
type Token = String
     --deriving (Eq, Ord, Show)
  
scanCalendar :: Parser Char [Token]
scanCalendar = greedy 

scanCrlf :: Parser Char





splitByString :: String -> String -> [String]
splitByString (a:b:c:d:xs) acc | a:b:c:d:[] == "\\r\\n" = error "noot" --acc : splitByString xs []
                               | otherwise = error (b:c:d:[]) -- splitByString (b:c:d) (acc ++ [a])
splitByString (x:xs) acc = splitByString xs (acc ++ [x])
splitByString [] acc = []
{-}

splitByReturn :: String -> [String]
splitByReturn xs = splitByString "\\r\\n"

splitPropertiesInPairs :: [String] -> [(String, String)]
splitPropertiesInPairs xs = map (splitByString xs) ':' (splitByReturn xs)
-}



parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do
                    handle <- openFile ReadMode path
                    _ <- hSetNewlinemode handle noNewlineTranslation
                    content <- hGetContents handle
                    return $ recognizeCalendar content


-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar cal = undefined

-- Exercise 10
countEvents :: Calendar -> Int
countEvents = undefined

findEvents :: DateTime -> Calendar -> [Event]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined
