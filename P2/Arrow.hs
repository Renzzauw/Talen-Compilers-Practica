module Arrow where

import Prelude hiding ((<*), (<$), Right, Left)
import ParseLib.Abstract
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.List
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Scanner as S
import Parser as P


type Space     =  Map Pos Contents
type Size      =  Int
type Pos       =  (Int, Int)
data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary

parseSpace :: Parser Char Space
parseSpace =
  do
    (mr,mc)  <-  parenthesised
                   ((,) <$> natural <* symbol ',' <*> natural) <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <-  replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
             zipWith (\ r cs  ->
             zipWith (\ c d   ->  ((r,c),d)) [0..] cs) [0..] css

spaces :: Parser Char String
spaces = greedy (satisfy isSpace)

contents :: Parser Char Contents
contents =
  choice (Prelude.map (\ (f,c) -> f <$ symbol c) contentsTable) <* spaces

contentsTable :: [(Contents,Char)]
contentsTable =
  [  (Empty,'.'),(Lambda,'\\'),(Debris,'%'),(Asteroid,'O'),(Boundary,'#')]

-- These three should be defined by you
type Ident = String
data Commands = NoneC | MultipleC Command Commands
data Heading = North | South | East | West

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String

-- Exercise 2
data Program = Program Rules
data Rules = NoneR | MultipleR Rule Rules 
data Rule = Rule String Commands
data Commands = NoCommand | MultipleC Command Commands
data Command = CGo | CTake | CMark | CNothing | CTurn Direction | CCase Direction Alts | CRule String
data Direction = CLeft | CRight | CFront
data Alts = NoneA | MultipleA Alt Alts
data Alt = Alt Pat Commands
data Pat = PEmpty | PLambda | PDebris | PAsteroid | PBoundary | PAny
         deriving    (Eq)

-- Exercise 3

-- zie parser

-- Exercise 4
{-
Left recursive parsers are ambiguous, and can result in more than one parse tree. Most parsers, like happy, throw errors when they spot LR segments in their parsers.
Happy has the GLR extention which can be triggered with the --glr flag, which allows the parsing of left recursive grammars.
It does that by exploring both paths simultaniously. As a result the parser does not construct a parse tree, but rather a 'parse graph'.
Right recursive parsers are not ambiguous and therefore parse normally.
TODO: Ik heb geen idee hoe het zit met combinators :(
-}
-- Exercise 5
-- We use records here so we do not have to pattern match on the insane amount of functions in the tuple
-- Env is a list of all known rules. Its contents are not important here, as only their name matters here
type Env = [String]

data PAlgebra r = PAlgebra{
                            pAlProgram          :: Env -> r -> r,
                            pAlNoRules          :: Env -> r,
                            pAlMultipleRules    :: Env -> r -> r -> r,
                            pAlRule             :: Env -> r -> r -> r,
                            pAlRuleID           :: Env -> String -> r,
                            pAlNoCommands       :: Env -> r,
                            pALMultipleCommands :: Env -> r -> r -> r,
                            pAlGo               :: Env -> r,
                            pAlTake             :: Env -> r,
                            pAlMark             :: Env -> r,
                            pAlNothing          :: Env -> r,
                            pAlTurn             :: Env -> r -> r,
                            pAlCase             :: Env -> r -> r -> r,
                            pAlCmdrule          :: Env -> r -> r,
                            pAlCmdruleID        :: Env -> String -> r,
                            pAlLeft             :: Env -> r,
                            pAlRight            :: Env -> r,
                            pAlFront            :: Env -> r,
                            pAlNoAlts           :: Env -> r,
                            pAlMultipleAlts     :: Env -> r -> r -> r,
                            pAlAlt              :: Env -> r -> r -> r,
                            pAlEmpty            :: Env -> r,
                            pAlLambda           :: Env -> r,
                            pAlDebris           :: Env -> r,
                            pAlAsteroid         :: Env -> r,
                            pAlBoundary         :: Env -> r, 
                            pAlAny              :: Env -> r 
                            }

foldProgram :: Env -> PAlgebra r -> Program -> r
foldProgram e pA (Program rules) = (pAlProgram pA) e $ foldRules e pA rules

foldRules :: Env -> PAlgebra r -> Rules -> r
foldRules e pA NoneR                  = pAlNoRules pA $ e
foldRules e pA (MultipleR rule rules) = (pAlMultipleRules pA) e (foldRule e pA rule) (foldRules e pA rules)

foldRule :: Env -> PAlgebra r -> Rule -> r
foldRule e pA (Rule id cmds) = (pAlRule pA) e ((pAlRuleID pA) e id) (foldCommands e pA cmds)

foldCommands :: Env -> PAlgebra r -> Commands -> r
foldCommands e pA NoneC                = pAlNoCommands pA $ e
foldCommands e pA (MultipleC cmd cmds) = (pALMultipleCommands pA) e (foldCommand e pA cmd) (foldCommands e pA cmds)

foldCommand :: Env -> PAlgebra r -> Command -> r
foldCommand e pA CGo              = pAlGo pA $ e
foldCommand e pA CTake            = pAlTake pA $ e
foldCommand e pA CMark            = pAlMark pA $ e
foldCommand e pA CNothing         = pAlNothing pA $ e
foldCommand e pA (CTurn dir)      = (pAlTurn pA) e (foldDirection e pA dir)
foldCommand e pA (CCase dir alts) = (pAlCase pA) e (foldDirection e pA dir) (foldAlts e pA alts)
foldCommand e pA (CRule name)     = (pAlCmdrule pA) e ((pAlCmdruleID pA) e name)

foldDirection :: Env -> PAlgebra r -> Direction -> r
foldDirection e pA Left  = pAlLeft pA $ e
foldDirection e pA Right = pAlRight pA$ e
foldDirection e pA Front = pAlFront pA$ e

foldAlts :: Env -> PAlgebra r -> Alts -> r
foldAlts e pA NoneA                = pAlNoAlts pA $ e
foldAlts e pA (MultipleA alt alts) = (pAlMultipleAlts pA) e (foldAlt e pA alt) (foldAlts e pA alts)

foldAlt :: Env -> PAlgebra r -> Alt -> r
foldAlt e pA (Alt pat cmds) = (pAlAlt pA) e (foldPat e pA pat) (foldCommands e pA cmds)

foldPat :: Env -> PAlgebra r -> Pat -> r
foldPat e pA PEmpty = pAlEmpty pA $ e
foldPat e pA PLambda = pAlLambda pA $ e
foldPat e pA PDebris = pAlDebris pA $ e
foldPat e pA PAsteroid = pAlAsteroid pA $ e
foldPat e pA PBoundary = pAlBoundary pA $ e
foldPat e pA PAny = pAlAny pA $ e

-- Exercise 6
-- TODO: alsjeblieft minder lang

-- Evaluates for the first 3 points. The 4th has to be done in a different algebra
pEvalAlgebra :: PAlgebra Bool
pEvalAlgebra = PAlgebra { pAlProgram = palprogram, pAlNoRules = palnorules, pAlMultipleRules = palmultiplerules, pAlRule = palrule, pAlRuleID = palruleid, pAlNoCommands = palnocommands, pALMultipleCommands = palmultiplecommands, pAlGo = palgo, pAlTake = paltake, pAlMark = palmark, pAlNothing = palnothing, pAlTurn = palturn, pAlCase = palcase, pAlCmdrule = palcmdrule, pAlCmdruleID = palcmdruleid, pAlLeft = palleft, pAlRight = palright, pAlFront = palfront, pAlNoAlts = palnoalts, pAlMultipleAlts = palmultiplealts, pAlAlt = palalt, pAlEmpty = palempty, pAlLambda = pallambda, pAlDebris = paldebris, pAlAsteroid = palasteroid, pAlBoundary = palboundary, pAlAny = palany}
             where palprogram          = (\env -> \x -> x)
                   palnorules          = (\env -> True)
                   palmultiplerules    = (\env -> \x -> \xs -> x && xs && elem "start" env)
                   palrule             = (\env -> \id -> \cmds -> id && cmds)
                   palruleid           = (\env -> \name -> not (elem name env))
                   palnocommands       = (\env -> True)
                   palmultiplecommands = (\env -> \x -> \xs -> x && xs)
                   palgo               = (\env -> True)
                   paltake             = (\env -> True)
                   palmark             = (\env -> True)
                   palnothing          = (\env -> True)
                   palturn             = (\env -> \x -> True)
                   palcase             = (\env x xs -> x && xs)
                   palcmdrule          = (\env -> \r -> r)
                   palcmdruleid        = (\env -> \name -> elem name env)
                   palleft             = (\env -> True)
                   palright            = (\env -> True)
                   palfront            = (\env -> True)
                   palnoalts           = (\env -> True)
                   palmultiplealts     = (\env -> \x -> \xs -> x || xs)
                   palalt              = (\env -> \x -> \y -> x && y)
                   palempty            = (\env -> False)
                   pallambda           = (\env -> False)
                   paldebris           = (\env -> False)
                   palasteroid         = (\env -> False)
                   palboundary         = (\env -> False)
                   palany              = (\env -> True)

-- Evaluates for the 4th point
pCaseAlgebra :: PAlgebra (Maybe [Pat])
pCaseAlgebra = PAlgebra { pAlProgram = palprogram, pAlNoRules = palnorules, pAlMultipleRules = palmultiplerules, pAlRule = palrule, pAlRuleID = palruleid, pAlNoCommands = palnocommands, pALMultipleCommands = palmultiplecommands, pAlGo = palgo, pAlTake = paltake, pAlMark = palmark, pAlNothing = palnothing, pAlTurn = palturn, pAlCase = palcase, pAlCmdrule = palcmdrule, pAlCmdruleID = palcmdruleid, pAlLeft = palleft, pAlRight = palright, pAlFront = palfront, pAlNoAlts = palnoalts, pAlMultipleAlts = palmultiplealts, pAlAlt = palalt, pAlEmpty = palempty, pAlLambda = pallambda, pAlDebris = paldebris, pAlAsteroid = palasteroid, pAlBoundary = palboundary, pAlAny = palany}
             where palprogram          = (\env -> \x -> x)
                   palnorules          = (\env -> Just [])
                   palmultiplerules    = (\env -> \x -> \xs -> combineMaybes x xs)
                   palrule             = (\env -> \id -> \cmds -> cmds)
                   palruleid           = (\env -> \name -> Just [])
                   palnocommands       = (\env -> Just [])
                   palmultiplecommands = (\env -> \x -> \xs -> combineMaybes x xs)
                   palgo               = (\env -> Just [])
                   paltake             = (\env -> Just [])
                   palmark             = (\env -> Just [])
                   palnothing          = (\env -> Just [])
                   palturn             = (\env -> \x -> Just [])
                   palcase             = (\env x xs -> xs)
                   palcmdrule          = (\env -> \r -> Just [])
                   palcmdruleid        = (\env -> \name -> Just [])
                   palleft             = (\env -> Just [])
                   palright            = (\env -> Just [])
                   palfront            = (\env -> Just [])
                   palnoalts           = (\env -> Just [])
                   palmultiplealts     = (\env -> \x -> \xs -> combineMaybes x xs)
                   palalt              = (\env -> \x -> \y -> nothingChecker x y)
                   palempty            = (\env -> Just [PEmpty])
                   pallambda           = (\env -> Just [PLambda])
                   paldebris           = (\env -> Just [PDebris])
                   palasteroid         = (\env -> Just [PAsteroid])
                   palboundary         = (\env -> Just [PBoundary])
                   palany              = (\env -> Just [PAny])

-- Helper function, to return a combination of two Maybe [Pat], It is kind of similar to a logical or
combineMaybes :: Maybe [Pat] -> Maybe [Pat] -> Maybe [Pat]
combineMaybes Nothing _           = Nothing
combineMaybes _ Nothing           = Nothing
combineMaybes (Just xs) (Just ys) = Just (union xs ys)

-- Helper function that checks whether the second value is Nothing or Just (something). In the case of the latter it returns the first value
nothingChecker :: Maybe [Pat] -> Maybe [Pat] -> Maybe [Pat]
nothingChecker _ Nothing  = Nothing
nothingChecker x (Just _) = x

-- Helper function that checks if all cases are caught, or whether there is a wildcard pattern inside
allFiveOrAny :: Maybe [Pat] -> Maybe [Pat]
allFiveOrAny (Just xs) | length xs == 5 || elem PAny xs = Just xs
                       | otherwise                      = Nothing
allFiveOrAny Nothing = error "It should always be a just!"

-- This algebra constructs an environment containing rules (their names, rather), which will be given as environment to the other two algebras
pEnvAlgebra :: PAlgebra Env
pEnvAlgebra = PAlgebra { pAlProgram = palprogram, pAlNoRules = palnorules, pAlMultipleRules = palmultiplerules, pAlRule = palrule, pAlRuleID = palruleid, pAlNoCommands = palnocommands, pALMultipleCommands = palmultiplecommands, pAlGo = palgo, pAlTake = paltake, pAlMark = palmark, pAlNothing = palnothing, pAlTurn = palturn, pAlCase = palcase, pAlCmdrule = palcmdrule, pAlCmdruleID = palcmdruleid, pAlLeft = palleft, pAlRight = palright, pAlFront = palfront, pAlNoAlts = palnoalts, pAlMultipleAlts = palmultiplealts, pAlAlt = palalt, pAlEmpty = palempty, pAlLambda = pallambda, pAlDebris = paldebris, pAlAsteroid = palasteroid, pAlBoundary = palboundary, pAlAny = palany}
             where palprogram          = (\env -> \x -> x)
                   palnorules          = (\env -> env)
                   palmultiplerules    = (\env -> \x -> \xs -> union x xs )
                   palrule             = (\env -> \ident -> \cmds -> ident)
                   palruleid           = (\env -> \name -> name : env)
                   -- The algebra does not do anything useful from here
                   palnocommands       = (\env -> env)
                   palmultiplecommands = (\env -> \x -> \xs -> env)
                   palgo               = (\env -> env)
                   paltake             = (\env -> env)
                   palmark             = (\env -> env)
                   palnothing          = (\env -> env)
                   palturn             = (\env -> \x -> env)
                   palcase             = (\env x xs -> env)
                   palcmdrule          = (\env -> \r -> env)
                   palcmdruleid        = (\env -> \name -> env)
                   palleft             = (\env -> env)
                   palright            = (\env -> env)
                   palfront            = (\env -> env)
                   palnoalts           = (\env -> env)
                   palmultiplealts     = (\env -> \x -> \xs -> env)
                   palalt              = (\env -> \x -> \y -> env)
                   palempty            = (\env -> env)
                   pallambda           = (\env -> env)
                   paldebris           = (\env -> env)
                   palasteroid         = (\env -> env)
                   palboundary         = (\env -> env)
                   palany              = (\env -> env)

      

-- Exercise 7
-- Size equals the width here, the height does not matter
printSpace :: Space -> String
printSpace space = concat $ map ((\x -> maybeEnter (fst x) (printElement (snd x)))) (L.assocs space)
                 where printElement :: Contents -> String
                       printElement Empty    = "."
                       printElement Lambda   = "\\"
                       printElement Debris   = "%"
                       printElement Asteroid = "O"
                       printElement Boundary = "#"
                       maybeEnter (a,b) s    = case a == width of
                                                 True -> s ++ "\n\r"
                                                 _    -> s
                       width = maximum $ map fst $ map fst $ L.toList space


-- TODO: dit kan eigenlijk pas als we weten hoe Alex/Happy werkt...
-- Exercise 8
toEnvironment :: String -> Environment
toEnvironment = checkIfPass $ P.happyParse $ S.alexScanTokens
              where env = foldProgram [] pEnvAlgebra 
                    checkIfPass p@(Program rules) | foldProgram (env p) pEvalAlgebra p && isJust (foldProgram (env p) pCaseAlgebra p) = makeEnv rules L.empty
                                                  | otherwise                                                                 = error "Program does not follow the rules!"

makeEnv :: Rules -> Environment -> Environment
makeEnv NoneR env                            = env
makeEnv (MultipleR (Rule id cmds) rules) env = L.insert id cmds (makeEnv rules env)

-- Exercise 9
step :: Environment -> ArrowState -> Step
step _ (ArrowState s p h NoneC) = Done s p h
step env (ArrowState s p h c)   = case getCurrent c of
                                  CGo          -> onNew
                                  CTake        -> Ok (ArrowState (L.adjust nowEmpty  p s) p h (newStack c))
                                  CMark        -> Ok (ArrowState (L.adjust nowLambda p s) p h (newStack c))
                                  CNothing     -> stayStill
                                  CTurn Front  -> stayStill
                                  CTurn d      -> Ok(ArrowState s p (getHeading h d) (newStack c))
                                  CCase d alts -> case find (\x -> pEquals (getLookPos d) (getPat x)) (foldAlts alts) of
                                                 Nothing -> Fail "There was no match!"
                                                 Just x  -> Ok (ArrowState s p h (prepend (getCom x) (newStack c)))
                                  CRule     r  -> case L.lookup r env of
                                                 Nothing -> Fail "Rule does not exist!"
                                                 Just x -> Ok (ArrowState (L.adjust nowEmpty p s) p h (prepend x (newStack c)))
                                where getCurrent (MultipleC c _) = c
                                      getCurrent NoneC           = error "None is handled earlier!"
                                      onNew = case s L.! newPos h of
                                              Empty  -> Ok (ArrowState s (newPos h) h (newStack c)) 
                                              Lambda -> Ok (ArrowState s (newPos h) h (newStack c))
                                              Debris -> Ok (ArrowState s (newPos h) h (newStack c))
                                              _      -> stayStill
                                      newPos North = (fst p, (snd p) + 1)
                                      newPos South = (fst p, (snd p) - 1)
                                      newPos East  = ((fst p) + 1, (snd p))
                                      newPos West  = ((fst p) - 1, (snd p))
                                      newStack (MultipleC _ r) = r
                                      stayStill  = Ok (ArrowState s p h (newStack c))
                                      nowEmpty = const Empty
                                      nowLambda = const Lambda
                                      getLookPos d = case L.lookup (newPos $ getHeading h d) s of
                                                     Nothing -> Boundary
                                                     Just a  -> a
                                      foldAlts NoneA = []
                                      foldAlts (MultipleA a ma) = a : foldAlts ma
                                      getPat (Alt p _) = p 
                                      getCom (Alt _ c) = c
                                      -- pEquals returns true if the pat and space tile are equal
                                      pEquals Empty PEmpty       = True
                                      pEquals Lambda PLambda     = True
                                      pEquals Debris PDebris     = True
                                      pEquals Asteroid PAsteroid = True
                                      pEquals Boundary PBoundary = True
                                      pEquals _ PAny             = True
                                      pEquals _ _                = False
                                      -- Put the first command string in front of the second
                                      prepend NoneC cmds               = cmds
                                      prepend (MultipleC cp cmdp) cmds = MultipleC cp $ prepend cmdp cmds

-- Helper function that can determine the next heading given a turn
-- Note that the "Front" direction does not get called here
getHeading :: Heading -> Direction -> Heading
getHeading North Left  = West
getHeading North Right = East
getHeading East Left   = North
getHeading East Right  = South
getHeading South Left  = East
getHeading South Right = West
getHeading West Left   = South
getHeading West Right  = North

-- Exercise 10
{-
REEEEEEEEEEEE cursion
-}
-- Exercise 11

interactive :: Environment -> ArrowState -> IO ()
interactive env a = intIterate env a

intIterate :: Environment -> ArrowState -> IO ()
intIterate env astate = case step env astate of
                        Done _ _ _                  -> return ()
                        Fail s                      -> putStrLn s
                        Ok as@(ArrowState s _ _ _ ) -> do
                                                       _ <- putStrLn "Here is your new map"
                                                       _ <- putStr $ printSpace s
                                                       _ <- putStrLn "type 'y' to continue, and 'n' to exit!"
                                                       checkChar env astate
checkChar :: Environment -> ArrowState -> IO ()
checkChar e a = do 
              c <- getChar
              case c of
                'n' -> return ()
                'y' -> intIterate e a
                _   -> do 
                       _ <- putStrLn "Please type 'y' or 'n'!"
                       checkChar e a
                                                   

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env a = iterate (step env a)
            where iterate (Done s p h) = (s, p, h)
                  iterate (Ok astate)  = iterate (step env astate)
                  iterate (Fail s)     = error s