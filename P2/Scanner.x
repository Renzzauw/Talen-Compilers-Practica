{
module Scanner where
}

%wrapper "basic"

$digit = 0-9                -- digits
$alpha = [a-zA-Z]           -- alphabetic characters
$symbols = [\-\>\,\.\_\;]   -- all other symbols occuring in the commands

tokens :-
    $white+                         ;                       -- Ignore whitespaces
    $digit+                         { \s -> read s }
 
    -- $alpha [$alpha $digit \_ \’]* { \s -> Var s } overbodig?

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
    -- "+"         { \s -> TSingleChar '+' }
    -- "-"         { \s -> TSingleChar '-' }
    -- $alpha [$alpha $digit]+ { \s -> TSingleChar (head s) }

{
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
    TUnderscore  
    --TIdent       
    deriving (Eq,Show, Read)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens

-- The Ident type:
--data TIdent = 
    --TMultiChar Char TIdent | 
    --TSingleChar Char
    --deriving (Eq,Show)  

main = do
    s <- getContents
    print (alexScanTokens s)
}
