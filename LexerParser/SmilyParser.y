{
module SmileyParser where
import AST
import ALexer
import Data.Maybe
import Data.Char
}

%tokentype { Token }

%token
    '['          { TokenLBrak }
    ']'          { TokenRBrak }
    '+'          { TokenAdd }
    '-'          { TokenMinus }
    '*'          { TokenTimes }
    '/'          { TokenDiv }
    '%'          { TokenMod }
    '~'          { TokenSgn }
    '#'          { TokenIndep }
    '('          { TokenOpen }
    ')'          { TokenClose }
    ','          { TokenComma }
    '@@'         { TokenConcat }
    '<'          { TokenLt }
    '>'          { TokenGt }
    '>='         { TokenGEt }
    '<='         { TokenLEt }
    '=='         { TokenEq }
    '/='         { TokenNEq }
    '||'         { TokenOR }
    '&&'         { TokenAND }
    '¬'          { TokenNOT }
    ';'          { TokenSeq }
    ':='         { TokenLet }
    True         { TokenTrue }
    False        { TokenFalse }
    D            { TokenD }
    Z            { TokenZ }
    least        { TokenLeast }
    largest      { TokenLargt }
    filter       { TokenFilter }
    max          { TokenMax }
    min          { TokenMin }
    sum          { TokenSum }
    count        { TokenCount }
    if           { TokenIf }
    then         { TokenThen }
    else         { TokenElse }
    accum        { TokenAccum }
    repeat       { TokenRepeat }
    until        { TokenUntil }
    NAME         { TokenName $$ $$ }
    INT          { TokenInt $$ }
    BOOL         { TokenBool $$ }
    COLL         { TokenColl $$ }


%right ';'
%left ':='
%right accum repeat until
%right if then else
%right '@@'
%right least largest filter max min sum count
%right D Z
%right '&&' '||' 
%left '¬'
%right '==' '<=' '>=' '<' '>' '/='
%right '#'
%left '~'
%left '+' '-'
%left '*' '/' '%'


%%

Cmd : NAME := Exp                       { Let $1 $3 } 
    | Cmd ';' Cmd                       { Seq $1 $3 } 
    | if Exp then Cmd else Cmd          { IfThenElse $2 $4 $6 }
    | accum Cmd until Cmd               { ACCUM $2 $4 }
    | repeat Cmd until Cmd              { REPUNT $2 $4 }

Atom : INT                              { INT $1 }
     | '[' CollList ']'                 { COLL $2 }
     | True                             { BOOL True }
     | False                            { BOOL False }

CollList : INT                      { $1 }
         | INT ',' CollList           { $1 : $2 }

{
happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)
}
