{
module Scanner where
}

%wrapper "basic"

$digit = 0-9                -- digits
$alpha = [a-zA-Z]           -- alphabetic characters
$symbols = [\-\>\,\.\_\;]   -- all other symbols occuring in the commands
$ident = [$alpha $digit \+ \-]

tokens :-
    $white+     ;                 -- Ignore whitespaces
    "--".*      ;                 -- Ignore comments untill the end of the line
    -- $digit+  { \s -> read s }

    -- Tokens
    "->"       { \s -> TArrow }
    "."        { \s -> TDot }
    ","        { \s -> TComma }
    "go"       { \s -> TGo }
    "take"     { \s -> TTake }
    "mark"     { \s -> TMark }
    "nothing"  { \s -> TNothing }
    "turn"     { \s -> TTurn }
    "case"     { \s -> TCase }
    "of"       { \s -> TOf }
    "end"      { \s -> TEnd }
    "left"     { \s -> TLeft }
    "right"    { \s -> TRight }
    "front"    { \s -> TFront }
    ';'        { \s -> TSemicolon }
    "Empty"    { \s -> TEmpty }
    "Lambda"   { \s -> TLambda }
    "Debris"   { \s -> TDebris }
    "Asteroid" { \s -> TAsteroid }
    "Boundary" { \s -> TBoundary }
    '_'        { \s -> TUnderscore }

    -- Idents
    $ident [$alpha $digit \+ \- ]+ { \s -> Tident (stringToTIdent s) }

{
-- Turn a variable name into a TIdent
stringToTIdent :: String -> TIdent
stringToTIdent (x:[]) = TSingleChar x
stringToTIdent (x:xs)  = TMultiChar x (stringToTIdent xs)

-- The Ident type:
data TIdent = 
    TMultiChar Char TIdent | 
    TSingleChar Char
    deriving (Eq, Show, Read)  

-- The token type:
data Token =
    TArrow       |   
    TDot         |
    TComma       |
    TGo          |
    TTake        |
    TMark        |
    TNothing     |
    TTurn        |
    TCase        |
    TOf          |
    TEnd         |
    TLeft        |
    TRight       |
    TFront       |
    TSemicolon   |
    TEmpty       |
    TLambda      |
    TDebris      |
    TAsteroid    |
    TBoundary    |
    TUnderscore  |
    Tident TIdent       
    deriving (Eq, Show, Read)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens

main = do
    s <- getContents
    print (alexScanTokens s)
}
