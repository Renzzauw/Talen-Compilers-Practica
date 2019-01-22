module CSharpLex where

import Data.Char
import Control.Monad
import ParseLib.Abstract
import Prelude hiding ((<*),(*>),(<$))


data Token = POpen    | PClose      -- parentheses     ()
           | SOpen    | SClose      -- square brackets []
           | COpen    | CClose      -- curly braces    {}
           | Comma    | Semicolon
           | KeyIf    | KeyElse
           | KeyWhile | KeyReturn
           | KeyFor
           | KeyTry   | KeyCatch
           | KeyClass | KeyVoid
           | StdType   String       -- the 8 standard types
           | Operator  String       -- the 15 operators
           | UpperId   String       -- uppercase identifiers
           | LowerId   String       -- lowercase identifiers
           | ConstInt  Int
           | ConstBool Int          -- Here 1 stands for true, and 0 stand for false
           | ConstChar Int          -- This is converted to ASCII values
           | Function [Token]
           deriving (Eq, Show)

-- ???           
keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x:_) | isLetter x = do ys <- greedy (satisfy isAlphaNum)
                                   guard (xs == ys)
                                   return ys
                 | otherwise  = token xs


greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty

-- All terminals as tuples of a token and its string/C# representation
terminals :: [(Token, String)]
terminals =
    [ ( POpen     , "("      )
    , ( PClose    , ")"      )
    , ( SOpen     , "["      )
    , ( SClose    , "]"      )
    , ( COpen     , "{"      )
    , ( CClose    , "}"      )
    , ( Comma     , ","      )
    , ( Semicolon , ";"      )
    , ( KeyIf     , "if"     )
    , ( KeyElse   , "else"   )
    , ( KeyWhile  , "while"  )
    , ( KeyFor    , "for"    )
    , ( KeyReturn , "return" )
    , ( KeyTry    , "try"    )
    , ( KeyCatch  , "catch"  )
    , ( KeyClass  , "class"  )
    , ( KeyVoid   , "void"   )
    ]

-- Lex white spaces
lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (satisfy isSpace)

-- Lex lowercase identifiers
lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

-- Lex uppercase identifiers
lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)

-- Lex constant ints
lexConstInt :: Parser Char Token
lexConstInt = (ConstInt . read) <$> greedy1 (satisfy isDigit)

--lexConstBool will try and read boolean values
lexConstBool :: Parser Char Token
lexConstBool = (ConstBool . boolToInt) <$> choice[token "true",token "false"]

-- Small helper function to convert booleans (in string form) to int
boolToInt :: String -> Int
boolToInt "true" = 1
boolToInt "false" = 0

--lexConstChar will lex a single character between apostrophes
lexConstChar :: Parser Char Token
lexConstChar = (ConstChar . ord) <$> pack apoLexer anySymbol apoLexer

-- Lex apostrophes
apoLexer :: Parser Char Char
apoLexer = satisfy (== '\'')

-- Lex Enums
lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

-- Lex terminals
lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]

-- List of all the datatypes
stdTypes :: [String]
stdTypes = ["int", "long", "double", "float", "byte", "short", "bool", "char"]

-- List of all the operators
operators :: [String]
operators = ["+", "-", "*", "/", "%", "&&", "||", "^", "<=", "<", ">=", ">", "==", "!=", "="]

-- Greedy lexing of tokens
lexToken :: Parser Char Token
lexToken = greedyChoice
             [ lexTerminal
             , lexEnum StdType stdTypes
             , lexEnum Operator operators
             , lexConstInt
             , lexLowerId
             , lexUpperId
             ]

-- This is the first scanner that converts every piece of useful information to tokens
lexicalScanner :: Parser Char [Token]
lexicalScanner = lexWhiteSpace *> greedy (lexToken <* lexWhiteSpace) <* eof

-- This is a second scanner, that removes all comments, and leaves the result as it is
commentScanner :: Parser Char String
commentScanner = (++) <$> reduceString <*> greedy anySymbol <* eof

reduceString :: Parser Char String
reduceString = concat <$> greedy (tilComment <* tilBreak)

-- Parser that detects single line comments
{-
lexComment :: Parser Char String
lexComment =  (++) <$> token "//" <*> tilBreak
-}

-- Function that parses until it hits a breakline
tilBreak :: Parser Char String
tilBreak = ((const []) <$> token "\n") <<|>  (:) <$> anySymbol <*> tilBreak

-- Function that parses until it hits a comment
tilComment :: Parser Char String
tilComment = ((const []) <$> token "//") <<|>  (:) <$> anySymbol <*> tilComment



sStdType :: Parser Token Token
sStdType = satisfy isStdType
    where isStdType (StdType _) = True
          isStdType _           = False

sUpperId :: Parser Token Token
sUpperId = satisfy isUpperId
    where isUpperId (UpperId _) = True
          isUpperId _           = False

sLowerId :: Parser Token Token
sLowerId = satisfy isLowerId
    where isLowerId (LowerId _) = True
          isLowerId _           = False

sConst :: Parser Token Token
sConst  = satisfy isConst
    where isConst (ConstInt  _) = True
          isConst (ConstBool _) = True
          isConst (ConstChar _) = True
          isConst _             = False

sOperator :: Parser Token Token
sOperator = satisfy isOperator
    where isOperator (Operator "=") = False
          isOperator (Operator _)   = True
          isOperator _              = False

sSemi :: Parser Token Token
sSemi =  symbol Semicolon

