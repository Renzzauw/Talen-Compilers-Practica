{
module Parser where   
import Scanner as S
}

%name lekkerParsen
%tokentype { Token }
%error { parseError }

%token       
        arrow       { S.TArrow }
        dot         { S.TDot }
        comma       { S.TComma }
        go          { S.TGo }
        take        { S.TTake }
        mark        { S.TMark }
        nothing     { S.TNothing }
        turn        { S.TTurn }
        case        { S.TCase }
        of          { S.TOf }
        end         { S.TEnd }
        left        { S.TLeft }
        right       { S.TRight }
        front       { S.TFront }
        semi        { S.TSemicolon }
        empty       { S.TEmpty }
        lambda      { S.TLambda }
        debris      { S.TDebris }
        asteroid    { S.TAsteroid }
        boundary    { S.TBoundary }
        undersc     { S.TUnderscore }
        ident       { S.Tident $$ }
            
    
%%

Program     : Rules                         { Program $1 }

Rule        : ident arrow Commands dot      { Rule (tIdentToString $1) $3 } 

Rules       :                               { NoneR }                    
            | Rules Rule                    { MultipleR $2 $1 }

Commands    :                               { NoCommand }
            | Commands comma Command        { MultipleC $3 $1 }

Command     : go                            { CGo }
            | take                          { CTake }
            | mark                          { CMark }
            | nothing                       { CNothing }
            | turn Direction                { CTurn $2 }
            | case Direction of Alts end    { CCase $2 $4 }
            | ident                         { CRule (tIdentToString $1) }

Direction   : left                          { CLeft }
            | right                         { CRight }
            | front                         { CFront }

Alts        :                               { NoneA }
            | Alts semi Alt                 { MultipleAlt $3 $1 }              

Alt         : Pat arrow Commands            { Alt $1 $3 }

Pat         : empty                         { PEmpty }
            | lambda                        { PLambda }
            | debris                        { PDebris }
            | asteroid                      { PAsteroid }
            | boundary                      { PBoundary }
            | undersc                       { PAny }

{

-- Exercise 2
data Program = Program Rules
             deriving (Show)

data Rule = Rule Identifier Commands
          deriving (Show)

data Rules = NoneR 
           | MultipleR Rule Rules
           deriving (Show)

data Commands = NoCommand
              | MultipleC Command Commands
              deriving (Show)  

data Command = CGo 
             | CTake 
             | CMark 
             | CNothing 
             | CTurn Direction 
             | CCase Direction Alts 
             | CRule Identifier
             deriving (Show)   
             
data Direction = CLeft 
               | CRight 
               | CFront
               deriving (Show)

type Identifier = String

-- Turn an identifier back into a string
tIdentToString :: TIdent -> String
tIdentToString (TSingleChar x) = [x]
tIdentToString (TMultiChar x xs)  = x : tIdentToString xs

data Alts = NoneA
          | MultipleAlt Alt Alts
          deriving (Show)

data Alt = Alt Pat Commands
         deriving (Show)

data Pat = PEmpty 
         | PLambda 
         | PDebris 
         | PAsteroid 
         | PBoundary 
         | PAny
         deriving (Eq, Show)

parseError :: [Token] -> a
parseError _ = error "Parsing error"
}