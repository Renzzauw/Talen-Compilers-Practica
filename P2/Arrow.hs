module Arrow where

import Prelude hiding ((<*), (<$))
import ParseLib.Abstract
import Data.Map (Map)
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)


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
type Ident = ()
type Commands = ()
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String

-- Exercise 2
data Program = Program [Rule]
data Rule = Rule Identifier [Command]
data Command = Go | Take | Mark | Nothing | Turn Direction | Case Direction [Alt] | Identifier
data Direction = Left | Right | Front
type Identifier = String
data Alt = Alt Pat [Command]
data Pat = PEmpty | PLambda | PDebris | PAsteroid | PBoundary | Any

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
type PAlgebra r = (r -> r)
-- Exercise 6
-- Exercise 7
-- Size equals the width here, the height does not matter here
printSpace :: Space -> Size -> String
printSpace space width = concat $ map ((\x -> maybeEnter (fst x) (printElement (snd x)))) (L.assocs space)
                       where printElement :: Contents -> String
                             printElement Empty    = "."
                             printElement Lambda   = "\\"
                             printElement Debris   = "%"
                             printElement Asteroid = "O"
                             printElement Boundary = "#"
                             maybeEnter (a,b) s    = case a == width of
                                                       True -> s ++ "\\n\\r"
                                                       _    -> s

-- Exercise 8
toEnvironment :: String -> Environment
-- TODO: dit kan eigenlijk pas als de vorige opdrachten ook werken...