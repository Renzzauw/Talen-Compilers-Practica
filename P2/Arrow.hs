module Arrow where

import Prelude hiding ((<*), (<$), Right, Left)
import ParseLib.Abstract
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.List
import System.IO
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.Tuple (swap)
import Scanner as S hiding (main)
import Parser as P hiding (main)

-- ******************* M A D E   B Y *******************
-- *          Hidde Veer 		    5721156						     *
-- *          Renzo Schindeler 	5964962						     *
-- *****************************************************

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
data Heading = North | South | East | West
             deriving (Show)

type Environment = Map Identifier Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String

tidentToString :: S.TIdent -> String 
tidentToString (TSingleChar c)     = [c]
tidentToString (TMultiChar c rest) = c : tidentToString rest

-- PLEASE NOTE THAT THE MAIN FUNCTION IS ALMOST AT THE BOTTOM AT EXERCISE 12

-- Exercise 1
{-
  See Scanner.x
-}

-- Exercise 2
{-
  See Parser.y at the bottom
-}

-- Exercise 3
{-
  See Parser.y
-}

-- Exercise 4
{-
Left recursive parsers are ambiguous, and can result in more than one parse tree. Most parsers, like happy, throw errors when they spot LR segments in their parsers.
Happy has the GLR extension which can be triggered with the --glr flag, which allows the parsing of left recursive grammars.
It does that by exploring both paths simultaniously. As a result the parser does not construct a parse tree, but rather a 'parse graph'.
Right recursive parsers are not ambiguous and therefore parse normally.

Parser combinators do not deal automatically with left recursion and will end up in endless recursion (until a stack overflow), so
to deal with left recursion you have to manually remove it by changing the grammar. Right recursion however, seems to always
work when using parser combinators, since they are not ambiguous.
-}

-- Exercise 5
-- We use records here so we do not have to pattern match on the insane amount of functions in the tuple
-- Env is a list of all known rules. Its contents are not important here, as only their name matters here
type Env = [String]

data PAlgebra r = PAlgebra{
                            pAlProgram          :: Env -> r -> r,
                            pAlRules            :: Env -> [r] -> r,                         
                            pAlRule             :: Env -> r -> r -> r,
                            pAlRuleID           :: Env -> String -> r,
                            pAlCommands         :: Env -> [r] -> r,
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
                            pAlCFront           :: Env -> r,                           
                            pAlAlts             :: Env -> [r] -> r,
                            pAlAlt              :: Env -> r -> r -> r,
                            pAlEmpty            :: Env -> r,
                            pAlLambda           :: Env -> r,
                            pAlDebris           :: Env -> r,
                            pAlAsteroid         :: Env -> r,
                            pAlBoundary         :: Env -> r, 
                            pAlAny              :: Env -> r 
                            }

-- foldProgram is the "main" fold                            
foldProgram :: Env -> PAlgebra r -> Program -> r
foldProgram e pA (Program rules) = (pAlProgram pA) e $ foldRules e pA rules

-- All of the other folds follow the general rule for folds.
foldRules :: Env -> PAlgebra r -> Rules -> r
foldRules e pA (rule:[])    = (foldRule e pA rule)
foldRules e pA r            = (pAlRules pA) e $ map (foldRule e pA) r

foldRule :: Env -> PAlgebra r -> Rule -> r
foldRule e pA (Rule id cmds) = (pAlRule pA) e ((pAlRuleID pA) e id) (foldCommands e pA cmds)

foldCommands :: Env -> PAlgebra r -> Commands -> r
foldCommands e pA (cmd:[])   = (foldCommand e pA cmd) -- not sure of dit klopt...
foldCommands e pA c          = (pAlCommands pA) e $ map (foldCommand e pA) c

foldCommand :: Env -> PAlgebra r -> Command -> r
foldCommand e pA CGo              = pAlGo pA $ e
foldCommand e pA CTake            = pAlTake pA $ e
foldCommand e pA CMark            = pAlMark pA $ e
foldCommand e pA CNothing         = pAlNothing pA $ e
foldCommand e pA (CTurn dir)      = (pAlTurn pA) e (foldDirection e pA dir)
foldCommand e pA (CCase dir alts) = (pAlCase pA) e (foldDirection e pA dir) (foldAlts e pA alts)
foldCommand e pA (CRule i)        = (pAlCmdrule pA) e ((pAlCmdruleID pA) e i)

foldDirection :: Env -> PAlgebra r -> Direction -> r
foldDirection e pA CLeft  = pAlLeft pA $ e
foldDirection e pA CRight = pAlRight pA $ e
foldDirection e pA CFront = pAlCFront pA $ e

foldAlts :: Env -> PAlgebra r -> Alts -> r
foldAlts e pA (alt:[])   = (foldAlt e pA alt)
foldAlts e pA a          = (pAlAlts pA) e $ map (foldAlt e pA) a

foldAlt :: Env -> PAlgebra r -> Alt -> r
foldAlt e pA (Alt pat cmds) = (pAlAlt pA) e (foldPat e pA pat) (foldCommands e pA cmds)

foldPat :: Env -> PAlgebra r -> Pat -> r
foldPat e pA PEmpty    = pAlEmpty pA $ e
foldPat e pA PLambda   = pAlLambda pA $ e
foldPat e pA PDebris   = pAlDebris pA $ e
foldPat e pA PAsteroid = pAlAsteroid pA $ e
foldPat e pA PBoundary = pAlBoundary pA $ e
foldPat e pA PAny      = pAlAny pA $ e

-- Exercise 6

-- Evaluates for the first 3 points. The 4th has to be done in a different algebra
pEvalAlgebra :: PAlgebra Bool
pEvalAlgebra = PAlgebra { pAlProgram = palprogram, pAlRules = palrules, pAlRule = palrule, pAlRuleID = palruleid, pAlCommands = palcommands, pAlGo = palgo, pAlTake = paltake, pAlMark = palmark, pAlNothing = palnothing, pAlTurn = palturn, pAlCase = palcase, pAlCmdrule = palcmdrule, pAlCmdruleID = palcmdruleid, pAlLeft = palleft, pAlRight = palright, pAlCFront = palCfront, pAlAlts = palalts, pAlAlt = palalt, pAlEmpty = palempty, pAlLambda = pallambda, pAlDebris = paldebris, pAlAsteroid = palasteroid, pAlBoundary = palboundary, pAlAny = palany}
             where palprogram          = (\env -> \x -> x && elem "start" env && length env == length (nub env))
                   palrules            = (\env -> \x -> foldr (&&) True x)
                   palrule             = (\env -> \id -> \cmds -> id && cmds)
                   palruleid           = (\env -> \name -> True)
                   palcommands         = (\env -> \x -> foldr (&&) True x)
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
                   palCfront           = (\env -> True)
                   palalts             = (\env -> \x -> foldr (&&) True x)
                   palalt              = (\env -> \x -> \y -> x && y)
                   palempty            = (\env -> True)
                   pallambda           = (\env -> True)
                   paldebris           = (\env -> True)
                   palasteroid         = (\env -> True)
                   palboundary         = (\env -> True)
                   palany              = (\env -> True)

-- Evaluates for the 4th point
pCaseAlgebra :: PAlgebra (Maybe [Pat])
pCaseAlgebra = PAlgebra { pAlProgram = palprogram, pAlRules = palrules, pAlRule = palrule, pAlRuleID = palruleid, pAlCommands = palcommands, pAlGo = palgo, pAlTake = paltake, pAlMark = palmark, pAlNothing = palnothing, pAlTurn = palturn, pAlCase = palcase, pAlCmdrule = palcmdrule, pAlCmdruleID = palcmdruleid, pAlLeft = palleft, pAlRight = palright, pAlCFront = palCfront, pAlAlts = palalts, pAlAlt = palalt, pAlEmpty = palempty, pAlLambda = pallambda, pAlDebris = paldebris, pAlAsteroid = palasteroid, pAlBoundary = palboundary, pAlAny = palany}
             where palprogram          = (\env -> \x -> x)                   
                   palrules            = (\env -> \xs -> foldr combineMaybes (Just []) xs)
                   palrule             = (\env -> \id -> \cmds -> cmds)
                   palruleid           = (\env -> \name -> Just [])
                   palcommands         = (\env -> \xs -> foldr combineMaybes (Just []) xs)
                   palgo               = (\env -> Just [])
                   paltake             = (\env -> Just [])
                   palmark             = (\env -> Just [])
                   palnothing          = (\env -> Just [])
                   palturn             = (\env -> \x -> Just [])
                   palcase             = (\env x xs -> allFiveOrAny xs)
                   palcmdrule          = (\env -> \r -> Just [])
                   palcmdruleid        = (\env -> \name -> Just [])
                   palleft             = (\env -> Just [])
                   palright            = (\env -> Just [])
                   palCfront           = (\env -> Just [])
                   palalts             = (\env -> \xs -> foldr combineMaybes (Just []) xs)
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
pEnvAlgebra = PAlgebra { pAlProgram = palprogram, pAlRules = palrules, pAlRule = palrule, pAlRuleID = palruleid, pAlCommands = palcommands, pAlGo = palgo, pAlTake = paltake, pAlMark = palmark, pAlNothing = palnothing, pAlTurn = palturn, pAlCase = palcase, pAlCmdrule = palcmdrule, pAlCmdruleID = palcmdruleid, pAlLeft = palleft, pAlRight = palright, pAlCFront = palCfront, pAlAlts = palalts, pAlAlt = palalt, pAlEmpty = palempty, pAlLambda = pallambda, pAlDebris = paldebris, pAlAsteroid = palasteroid, pAlBoundary = palboundary, pAlAny = palany}
             where palprogram          = (\env -> \x -> x)
                   palrules            = (\env -> \x -> foldr union [] x)
                   palrule             = (\env -> \ident -> \cmds -> ident)
                   palruleid           = (\env -> \name -> name : env)
                   -- The algebra does not do anything useful from here
                   palcommands         = (\env -> \x -> foldr union [] x)
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
                   palCfront           = (\env -> env)
                   palalts             = (\env -> \x -> foldr union [] x)
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
printSpace space = concat $ map ((\x -> maybeEnter (fst x) (printElement (snd x)))) (L.assocs (L.mapKeys swap space))
                 where printElement Empty    = "."
                       printElement Lambda   = "\\"
                       printElement Debris   = "%"
                       printElement Asteroid = "O"
                       printElement Boundary = "#"
                       maybeEnter (a,b) s    = case b == width of
                                                 True -> s ++ "\r\n"
                                                 _    -> s
                       width = maximum $ map fst $ map fst $ L.toList space

-- Exercise 8
toEnvironment :: String -> Environment
toEnvironment s = checkIfPass $ P.lekkerParsen $ S.lekkerLexen s
              where env = foldProgram [] pEnvAlgebra 
                    checkIfPass p@(Program rules) | foldProgram (env p) pEvalAlgebra p && isJust (foldProgram (env p) pCaseAlgebra p) = makeEnv rules L.empty
                                                  | otherwise                                                                 = error "Program does not follow the rules!"

makeEnv :: Rules -> Environment -> Environment
makeEnv [] env                     = env
makeEnv ((Rule id cmds):rules) env = L.insert id cmds (makeEnv rules env)

-- Exercise 9
step :: Environment -> ArrowState -> Step
step _ (ArrowState s p h []) = Done s p h
step env (ArrowState s p h c)   = case getCurrent c of
                                  CGo          -> onNew
                                  CTake        -> Ok (ArrowState (L.adjust nowEmpty  p s) p h (newStack c))
                                  CMark        -> Ok (ArrowState (L.adjust nowLambda p s) p h (newStack c))
                                  CNothing     -> stayStill
                                  CTurn d      -> Ok(ArrowState s p (getHeading h d) (newStack c))
                                  CCase d alts -> case find (\x -> pEquals (getLookPos d) (getPat x)) (foldAlts alts) of
                                                 Nothing -> Fail "There was no match!"
                                                 Just x  -> Ok (ArrowState s p h (prepend (getCom x) (newStack c)))
                                  CRule r      -> case L.lookup r env of
                                                 Nothing -> Fail "Rule does not exist!"
                                                 Just x -> Ok (ArrowState (L.adjust nowEmpty p s) p h (prepend x (newStack c)))
                                where getCurrent (c:_) = c
                                      getCurrent []    = error "None is handled earlier!"
                                      onNew = case L.lookup (newPos h) s of
                                             Just Empty  -> Ok (ArrowState s (newPos h) h (newStack c)) 
                                             Just Lambda -> Ok (ArrowState s (newPos h) h (newStack c))
                                             Just Debris -> Ok (ArrowState s (newPos h) h (newStack c))
                                             _           -> stayStill
                                      newPos North = (fst p, (snd p) + 1)
                                      newPos South = (fst p, (snd p) - 1)
                                      newPos East  = ((fst p) + 1, (snd p))
                                      newPos West  = ((fst p) - 1, (snd p))
                                      newStack  (_:r) = r
                                      stayStill  = Ok (ArrowState s p h (newStack c))
                                      nowEmpty = const Empty
                                      nowLambda = const Lambda
                                      getLookPos d = case L.lookup (newPos $ getHeading h d) s of
                                                     Nothing -> Boundary
                                                     Just a  -> a
                                      foldAlts [] = []
                                      foldAlts (a:ma) = a : foldAlts ma
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
                                      -- Put the first command string in Cfront of the second
                                      prepend [] cmds               = cmds
                                      prepend (cp:cmdp) cmds = (cp:(prepend cmdp cmds)) -- not sure?

-- Helper function that can determine the next heading given a turn
-- Note that the "CFront" direction does not get called here
getHeading :: Heading -> Direction -> Heading
getHeading North CFront = North
getHeading North CLeft  = West
getHeading North CRight = East
getHeading East CFront  = East
getHeading East CLeft   = North
getHeading East CRight  = South
getHeading South CFront = South
getHeading South CLeft  = East
getHeading South CRight = West
getHeading West CFront  = West
getHeading West CLeft   = South
getHeading West CRight  = North

-- Exercise 10
{-
The location of the recursion does have effect on the length of the stack. Consider a stack consisting of a series of commands, where on of them is a rule.
If the rule is at the bottom of the stack, iterating will cause the stack to decrease so that only the rule remains at the end. The rule will add new commands to the stack, causing the stack to entirely exists out of these new commands.
However, when a rule is placed at a different (random) location, the new commands will be added to the stack, while some old commands still remain on the stack.
The stack will then consist out of the commands added by the rule on top, and the remaining commands from before the rule call at the bottom.
Therefore, the lower the rule is on the stack, the shorter the stack will be after the rule call.
-}

-- Exercise 11
{- 
  This is the main function of the program.
  To run it, type in the console: main "examples/_.arrow" "examples/_.space" (x,y) North/South/East/West 1/2
  This function will run the program with an .arrow and .space file given their filepaths,
  a start position (x,y) where x and y are of type Int, and a runmode 1 (interactive mode)
  or 2 (batch mode). 

  please note that mode 1 (interactive) might need quite some times of clicking on 'y' before the map seems to change.
-}

--        .arrow      .space    (x,y)  heading    1/2    
main :: FilePath -> FilePath -> Pos -> Heading -> Int -> IO ()
main path1 path2 pos heading mode = do
                                    s1         <- readFile path1                     -- read .arrow file
                                    s2         <- readFile path2                     -- read .space file                                      
                                    let env    = toEnvironment s1                    -- convert to an Environment
                                    let space  = fst $ head $ parse parseSpace s2    -- convert to a Space
                                    let stack  = env L.! "start"                     -- get the stack
                                    let initial = ArrowState space pos heading stack -- create the arrowState
                                    case mode of
                                         1 ->  interactive env initial               -- mode 1 (interactive)
                                         2 ->  printBatch (batch env initial)        -- mode 2 (batch)
                                         _ ->  error "invalid run mode of main"      -- invalid

printBatch :: (Space, Pos, Heading) -> IO ()
printBatch (space, pos, heading) = do
                                   _ <- putStrLn "Here is your new map"
                                   _ <- putStrLn $ printSpace space
                                   _ <- putStrLn "Here is your new pos"
                                   _ <- putStrLn $ show pos
                                   _ <- putStrLn "Here is your new heading"
                                   _ <- putStrLn $ show heading
                                   return ()

interactive :: Environment -> ArrowState -> IO ()
interactive env a = intIterate env a

intIterate :: Environment -> ArrowState -> IO ()
intIterate env astate = case step env astate of
                        Done _ _ _                  -> return ()
                        Fail s                      -> putStrLn s
                        Ok as@(ArrowState s _ _ _ ) -> do
                                                       _ <- putStrLn "Here is your new map"
                                                       _ <- putStrLn $ printSpace s
                                                       _ <- putStrLn "type 'y' to continue, and 'n' to exit!"
                                                       checkChar env astate
checkChar :: Environment -> ArrowState -> IO ()
checkChar e a = do 
              c <- getChar
              case c of
                'n' -> return ()
                'y' -> iteration
                _   -> do 
                       _ <- putStrLn "Please type 'y' or 'n'!"
                       checkChar e a
              where iteration = case (step e a) of
                                (Done _ _ _) -> putStrLn "You are done!"
                                (Ok next)    -> intIterate e next
                                (Fail _)     -> error "This should throw an error earlier!"
                                                   

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env a = iterate (step env a)
            where iterate (Done s p h) = (s, p, h)
                  iterate (Ok astate)  = iterate (step env astate)
                  iterate (Fail s)     = error s
                  