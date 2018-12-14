{
    module Parser where
    
    import Scanner as S
}

%name happyParse
%tokentype { Token }
%error { parseError }

%token
        rule        { tRule }
        command     { tCmd }
        
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
        ';'         { S.TSemicolon }
        empty       { S.TEmpty }
        lambda      { S.TLambda }
        debris      { S.TDebris }
        asteroid    { S.TAsteroid }
        boundary    { S.TBoundary }
        any         { S.TUnderscore }
        ident       { S.TIdent }
            
    
%%

Program     : Rules                         { Program $1 }

Rules       : Rule                          { [$1] }                    
            | Rule Rules                    { $2 : $1 }

Rule        : Identifier arrow Commands dot  { Rule $1 $3 } 

Commands    : Command                       { [$1] }
            | Command Commands              { $2 : $1 }

Command     : go                            { Go }
            | take                          { Take }
            | mark                          { Mark }
            | nothing                       { Nothing }
            | turn Direction                { Turn $2 }
            | case Direction of Alts end   { Case $2 $4}

Alts        : Alt                           { [$1] }
            | Alt Alts                      { $2 : $1 }              

Alt         : Identifier arrow Commands     { $1 }

Direction   : left                          { Left }
            | right                         { Right }
            | front                         { Front }

Identifier  : ident                         { $1 }

Pat         : empty                         { Empty }
            | lambda                        { Lambda }
            | debris                        { Debris }
            | asteroid                      { Asteroid }
            | boundary                      { Boundary }
            | Identifier                    { $1 }
{

-- Exercise 2
data Program = Program [Rule]
data Rule = Rule Identifier [Command]
data Command = Go | Take | Mark | Nothing | Turn Dir | Case Dir [Alt] | Identifier
data Direction = Left | Right | Front
type Identifier = String
data Alt = Alt Pat [Command]
data Pat = Empty | Lambda | Debris | Asteroid | Boundary | Any


parseError :: [Token] -> a
parseError _ = error "Parsing error"
}