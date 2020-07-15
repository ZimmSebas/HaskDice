{
module SmilyParser where
import AST
import ALexer
import Data.Maybe
import Data.Char
}

%name smilyparse
%tokentype { Token }
%error { happyError }

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
    '||'         { TokenOr }
    '&&'         { TokenAnd }
    '¬'          { TokenNot }
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
    NAME         { TokenName $$ }
    INT          { TokenInt $$ }


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
%left NEG


%%


Cmd   : Cmd ';' Cmd                       { Seq $1 $3 } 
      | NAME ':=' Exp                     { Let (text $1) (Expr $3) }
      | Exp                               { Expr $1 }
      | filter Fop Atom                   { Filter $2 $3 }
      | if Exp then Cmd else Cmd          { IfThenElse $2 $4 $6 }
      | accum Cmd until Cmd               { ACCUM $2 $4 }
      | repeat Cmd until Cmd              { REPUNT $2 $4 }

Exp   : Exp '@@' Exp                      { CONCAT $1 $3 } 
      | least Atom Exp                    { LEAST $2 $3 }
      | largest Atom Exp                  { LARGT $2 $3 }
      | max Exp                           { MAX $2 }
      | min Exp                           { MIN $2 }
      | sum Exp                           { SUM $2 }
      | count Exp                         { COUNT $2 }
      | Atom D Atom                       { D $1 $3 }
      | Atom Z Atom                       { Z $1 $3 }
      | Exp '&&' Exp                      { AND $1 $3 }
      | Exp '||' Exp                      { OR $1 $3 }
      | '¬' Exp                           { NOT $2 }
      | Exp '==' Exp                      { Eq $1 $3 }
      | Exp '/=' Exp                      { NEq $1 $3 }
      | Exp '<=' Exp                      { LEt $1 $3 }
      | Exp '>=' Exp                      { GEt $1 $3 }
      | Exp '<' Exp                       { Lt $1 $3 }
      | Exp '>' Exp                       { Gt $1 $3 }
      | Exp '#' Exp                       { INDEP $1 $3 }
      | '~' Exp                           { SGN $2 }
      | Exp '+' Exp                       { ADD $1 $3 }
      | Exp '-' Exp                       { MINUS $1 $3 }
      | Exp '*' Exp                       { TIMES $1 $3 }
      | Exp '/' Exp                       { DIV $1 $3 }
      | Exp '%' Exp                       { MOD $1 $3 }
      | '-' Exp %prec NEG                 { UMINUS $2 }
      | NAME                              { Var (text $1) }

Fop   :: { FilOp }
      : '(' '<' Atom ')'                  { Grtth $3 }
      | '(' '>' Atom ')'                  { Lowth $3 }
      | '(' '>=' Atom ')'                 { GrtEqt $3 }
      | '(' '<=' Atom ')'                 { LowEqt $3 }
      | '(' '==' Atom ')'                 { Equal $3 }
      | '(' '/=' Atom ')'                 { NEqual $3 }
      
Atom   : INT                              { INT $1 }
       | '[' Coll ']'                     { COLL $1 }
       | True                             { BOOL True }
       | False                            { BOOL False }

Coll   : INT                              { $1 }
       | INT ',' Coll                     { $1 $2 }


{                 

happyError :: [Token] -> a
happyError [] = error "No idea"
happyError (t:ts) = error "Parse Error " ++ (show (tokenPos t)) ++ "."


}
