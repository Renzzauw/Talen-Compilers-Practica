{
module Parser where   
import Scanner as S
}

%name lekkerParsen
%tokentype { Token }
%error { parseError }

%token
        rule        { tRule }
        command     { tCmd }
        
        "->"        { S.TArrow }
        '.'         { S.TDot }
        ','         { S.TComma }
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
        ';'         { S.TSemicolon }
        empty       { S.TEmpty }
        lambda      { S.TLambda }
        debris      { S.TDebris }
        asteroid    { S.TAsteroid }
        boundary    { S.TBoundary }
        '_'         { S.TUnderscore } -- any
        ident       { S.Tident $$ }
            
    
%%

Program     : Rules                         { Program $1 }

Rule        : Identifier "->" Commands '.'  { Rule $1 $3 } 

Rules       : {- Empty -}                   { NoneR }                    
            | Rules Rule                    { MultipleR $2 $1 }

Commands    : {- Empty -}                   { NoCommand }
            | Commands ',' Command          { MultipleC $3 $1 }

Command     : go                            { CGo }
            | take                          { CTake }
            | mark                          { CMark }
            | nothing                       { CNothing }
            | turn Direction                { CTurn $2 }
            | case Direction of Alts end    { CCase $2 $4}

Direction   : left                          { CLeft }
            | right                         { CRight }
            | front                         { CFront }

Alts        : {- Empty -}                   { EmptyA }
            | Alt ';' Alts                  { MultipleAlt $3 $1 }              

Alt         : Pat "->" Commands             { Alt $1 $3 }

Pat         : empty                         { PEmpty }
            | lambda                        { PLambda }
            | debris                        { PDebris }
            | asteroid                      { PAsteroid }
            | boundary                      { PBoundary }
            | '_'                           { PAny }

Identifier  : ident                         { $1 }
{

-- Exercise 2
data Program = Program Rules

data Rule = Rule Identifier Commands

data Rules = NoneR 
           | MultipleR Rule Rules













data Commands = NoCommand
              | MultipleC Command Commands

data Command = CGo 
             | CTake 
             | CMark 
             | CNothing 
             | CTurn Direction 
             | CCase Direction Alts 
             | Identifier

data Direction = CLeft 
               | CRight 
               | CFront

type Identifier = String

data Alts = NoneA
          | MultipleAlt Alt Alts

data Alt = Alt Pat Commands

data Pat = PEmpty 
         | PLambda 
         | PDebris 
         | PAsteroid 
         | PBoundary 
         | PAny


parseError :: [Token] -> a
parseError _ = error "Parsing error"
}