{module ALexer where
import AST}

%wrapper "posn" 

$digit = 0-9              -- digits
$alpha = [a-zA-Z]         -- alphabetic characters
$bool = [true-false]      -- booleans

tokens :- 
    $white+               ;
    "--".*                ;
    "int"                 { \pos str -> TokenTInt pos }
    "bool"                { \pos str -> TokenTBool pos }
    "collection"          { \pos str -> TokenTColl pos }
    "&&"                  { \pos str -> TokenAND pos }
    "||"                  { \pos str -> TokenOR pos }
    $digit+			      { \pos str -> TokenInt pos (read str) }
    $bool+			      { \pos str -> TokenBool pos (read str) }
    ":="				  { \pos str -> TokenLet pos }
    \[                    { \pos str -> TokenLBrak pos }
    \]                    { \pos str -> TokenRBrak pos }
    \(                    { \pos str -> TokenOpen pos }
    \)                    { \pos str -> TokenClose pos }
    \+                    { \pos str -> TokenAdd pos }
    \-                    { \pos str -> TokenMinus pos }
    \*                    { \pos str -> TokenTimes pos }
    \/                    { \pos str -> TokenDiv pos }
    \%                    { \pos str -> TokenMod pos }
    \<                    { \pos str -> TokenLt pos }
    \>                    { \pos str -> TokenGt pos }
    ">="                  { \pos str -> TokenGEt pos }
    "<="                  { \pos str -> TokenLEt pos }
    "=="                    { \pos str -> TokenEq pos }
    "!="                  { \pos str -> TokenNEq pos }
    \#                    { \pos str -> TokenIndep pos }
    "D"                   { \pos str -> TokenD pos }
    "Z"                   { \pos str -> TokenZ pos }
    \#                    { \pos str -> TokenIndep pos }
    "least"               { \pos str -> TokenLeast pos }
    "largest"             { \pos str -> TokenLargt pos }
    "&&"                  { \pos str -> TokenConcat pos }
    "filter"              { \pos str -> TokenFilter pos }
    "max"                 { \pos str -> TokenMAX pos }
    "min"                 { \pos str -> TokenMIN pos }
    "sum"                 { \pos str -> TokenSUM pos }
    "count"               { \pos str -> TokenCOUNT pos }
    "and"                 { \pos str -> TokenAND pos }
    "or"                  { \pos str -> TokenOR pos }
    \¬                    { \pos str -> TokenNOT pos }
    "is empty"            { \pos str -> TokenIsEmpty pos }
    \;                    { \pos str -> TokenSeq pos }

{
data Token = TokenLBrak       {position :: AlexPosn} -- [
    | TokenRBrak       {position :: AlexPosn} -- ]
    | TokenLet         {position :: AlexPosn} -- :=
    | TokenNeg         {position :: AlexPosn} -- ¬
    | TokenAdd         {position :: AlexPosn} -- +
    | TokenMinus       {position :: AlexPosn} -- -
    | TokenTimes       {position :: AlexPosn} -- *
    | TokenDiv         {position :: AlexPosn} -- /
    | TokenMod         {position :: AlexPosn} -- %
    | TokenSgn         {position :: AlexPosn} -- ~
    | TokenIndep       {position :: AlexPosn} -- #
    | TokenOpen        {position :: AlexPosn} -- (
    | TokenClose       {position :: AlexPosn} -- )
    | TokenTInt        {position :: AlexPosn}
    | TokenTBool       {position :: AlexPosn}
    | TokenTColl       {position :: AlexPosn}
    | TokenFopGt       {position :: AlexPosn}
    | TokenFopLt       {position :: AlexPosn}
    | TokenFopGEt      {position :: AlexPosn}
    | TokenFopLEt      {position :: AlexPosn}
    | TokenFopEq       {position :: AlexPosn}
    | TokenFopNEq      {position :: AlexPosn}
    | TokenD           {position :: AlexPosn}
    | TokenZ           {position :: AlexPosn}
    | TokenINT         {position :: AlexPosn}
    | TokenCOLL        {position :: AlexPosn}
    | TokenBOOL        {position :: AlexPosn}
    | TokenVar         {position :: AlexPosn}
    | TokenLeast       {position :: AlexPosn}
    | TokenLargt       {position :: AlexPosn}
    | TokenFilter      {position :: AlexPosn}
    | TokenConcat      {position :: AlexPosn}
    | TokenMAX         {position :: AlexPosn}
    | TokenMIN         {position :: AlexPosn}
    | TokenSUM         {position :: AlexPosn}
    | TokenCOUNT       {position :: AlexPosn}
    | TokenLt          {position :: AlexPosn} -- <
    | TokenGt          {position :: AlexPosn} -- >
    | TokenGEt         {position :: AlexPosn} -- >=
    | TokenLEt         {position :: AlexPosn} -- <=
    | TokenEq          {position :: AlexPosn} -- ==
    | TokenNEq         {position :: AlexPosn} -- /=
    | TokenAND         {position :: AlexPosn} -- &&
    | TokenOR          {position :: AlexPosn} -- ||
    | TokenNOT         {position :: AlexPosn} -- ¬
    | TokenIsEmpty     {position :: AlexPosn}
    | TokenExpr        {position :: AlexPosn}
    | TokenSeq         {position :: AlexPosn} -- ;
    | TokenIfThenElse  {position :: AlexPosn}
    | TokenACCUM       {position :: AlexPosn}
    | TokenREPUNT      {position :: AlexPosn}
    | TokenInt         {position :: AlexPosn, value :: Int}
    | TokenBool        {position :: AlexPosn, valueB :: Bool}
    | TokenCollection  {position :: AlexPosn, valueC :: [Int]}
    | TokenName        {position :: AlexPosn, text :: String}
 deriving (Eq, Show)

lexer = alexScanTokens
}
