{
    module Parser where
}

%name happyParse
%tokentype { Token }
%error { parseError }

%token
        '.'                 { tDot }
        ','                 { tComma}
        rule                { tRule }
        command             { tCmd }
        go                  { tGo }
        take                { tTake }
        mark                { tMark }
        nothing             { tNothing }
        turn                { tTurn }
        case                { tCase }
        of                  { tOf }
        end                 { tEnd }
        left                { tLeft }
        right               { tLeft }
        front               { tLeft }
        ';'                 { tSemicolon }
        alt                 { tokenAlt }
        empty               { tokenEmpty }
        lambda              { tokenLambda }
        debris              { tokenDebris }
        asteroid            { tokenAsteroid }
        boundary            { tokenBoundary }
        ident               { tokenIdent }
        "->"                { tArrow }

$$

Program     : [Rule]                        { Program $1 }

Rule        : Identifier "->" [Command] '.' { Rule $1 $3 } 

Command     : go                            { Go }
            | take                          { Take }
            | mark                          { Mark }
            | nothing                       { Nothing }
            | turn Direction                { Turn $2 }
            | case Direction of [Alt] end   { Case $2 $4}

Alt         : Identifier "->" [Command]     { $1 }

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