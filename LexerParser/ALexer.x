{module ALexer where
import AST}

%wrapper "posn" 

$digit = 0-9              -- digits
$alpha = [a-zA-Z]         -- alphabetic characters
$lower = [a-z]            -- alphabetic lowercase characters

tokens :- 
    $white+                             ;
    "--".*                              ;
    \[                                  { \pos str -> TokenLBrak pos }
    \]                                  { \pos str -> TokenRBrak pos }
    \(                                  { \pos str -> TokenOpen pos }
    \)                                  { \pos str -> TokenClose pos }
    \,                                  { \pos str -> TokenComma pos }
    \+                                  { \pos str -> TokenAdd pos }
    \-                                  { \pos str -> TokenMinus pos }
    \*                                  { \pos str -> TokenTimes pos }
    \/                                  { \pos str -> TokenDiv pos }
    \%                                  { \pos str -> TokenMod pos }
    $digit+                             { \pos str -> TokenInt pos (read str) }
    \<                                  { \pos str -> TokenLt pos }
    \>                                  { \pos str -> TokenGt pos }
    ">="                                { \pos str -> TokenGEt pos }
    "<="                                { \pos str -> TokenLEt pos }
    "=="                                { \pos str -> TokenEq pos }
    "/="                                { \pos str -> TokenNEq pos }
    "&&"                                { \pos str -> TokenAnd pos }
    "||"                                { \pos str -> TokenOr pos }
    \¬                                  { \pos str -> TokenNot pos }
    "True"                              { \pos str -> TokenTrue pos }
    "False"                             { \pos str -> TokenFalse pos }
    \#                                  { \pos str -> TokenIndep pos }
    "D"                                 { \pos str -> TokenD pos }
    "Z"                                 { \pos str -> TokenZ pos }
    "least"                             { \pos str -> TokenLeast pos }
    "largest"                           { \pos str -> TokenLargt pos }
    "@@"                                { \pos str -> TokenConcat pos }
    "filter"                            { \pos str -> TokenFilter pos }
    "max"                               { \pos str -> TokenMax pos }
    "min"                               { \pos str -> TokenMin pos }
    "sum"                               { \pos str -> TokenSum pos }
    "count"                             { \pos str -> TokenCount pos }
    ":="                                { \pos str -> TokenLet pos }
    "if"                                { \pos str -> TokenIf pos }
    "then"                              { \pos str -> TokenThen pos }
    "else"                              { \pos str -> TokenElse pos }
    "is empty"                          { \pos str -> TokenIsEmpty pos }
    \;                                  { \pos str -> TokenSeq pos }
    "accum"                             { \pos str -> TokenAccum pos }
    "repeat"                            { \pos str -> TokenRepeat pos }
    "until"                             { \pos str -> TokenUntil pos }
    $lower [$alpha $digit \_ \']*       { \pos str -> TokenName pos str}


{
data Token = TokenLBrak       {position :: AlexPosn} -- [
           | TokenRBrak       {position :: AlexPosn} -- ]
           | TokenLet         {position :: AlexPosn} -- :=
           | TokenAdd         {position :: AlexPosn} -- +
           | TokenMinus       {position :: AlexPosn} -- -
           | TokenTimes       {position :: AlexPosn} -- *
           | TokenDiv         {position :: AlexPosn} -- /
           | TokenMod         {position :: AlexPosn} -- %
           | TokenSgn         {position :: AlexPosn} -- ~
           | TokenIndep       {position :: AlexPosn} -- #
           | TokenOpen        {position :: AlexPosn} -- (
           | TokenClose       {position :: AlexPosn} -- )
           | TokenComma       {position :: AlexPosn} -- ,
           | TokenFopGt       {position :: AlexPosn}
           | TokenFopLt       {position :: AlexPosn}
           | TokenFopGEt      {position :: AlexPosn}
           | TokenFopLEt      {position :: AlexPosn}
           | TokenFopEq       {position :: AlexPosn}
           | TokenFopNEq      {position :: AlexPosn}
           | TokenD           {position :: AlexPosn}
           | TokenZ           {position :: AlexPosn}
           | TokenLeast       {position :: AlexPosn}
           | TokenLargt       {position :: AlexPosn}
           | TokenFilter      {position :: AlexPosn}
           | TokenConcat      {position :: AlexPosn} -- @@
           | TokenMax         {position :: AlexPosn}
           | TokenMin         {position :: AlexPosn}
           | TokenSum         {position :: AlexPosn}
           | TokenCount       {position :: AlexPosn}
           | TokenLt          {position :: AlexPosn} -- <
           | TokenGt          {position :: AlexPosn} -- >
           | TokenGEt         {position :: AlexPosn} -- >=
           | TokenLEt         {position :: AlexPosn} -- <=
           | TokenEq          {position :: AlexPosn} -- ==
           | TokenNEq         {position :: AlexPosn} -- /=
           | TokenAnd         {position :: AlexPosn} -- &&
           | TokenOr          {position :: AlexPosn} -- ||
           | TokenNot         {position :: AlexPosn} -- ¬
           | TokenIsEmpty     {position :: AlexPosn}
           | TokenSeq         {position :: AlexPosn} -- ;
           | TokenIf          {position :: AlexPosn}
           | TokenThen        {position :: AlexPosn}
           | TokenElse        {position :: AlexPosn}
           | TokenAccum       {position :: AlexPosn}
           | TokenRepeat      {position :: AlexPosn}
           | TokenUntil       {position :: AlexPosn}
           | TokenTrue        {position :: AlexPosn}
           | TokenFalse       {position :: AlexPosn}
           | TokenInt         {position :: AlexPosn, value :: Int}
           | TokenName        {position :: AlexPosn, text :: String}
 deriving (Eq, Show)

lexer = alexScanTokens
}

--     Tokens i'll probably never use
--     | TokenBool        {position :: AlexPosn, valueB :: Bool}
--     | TokenCollection  {position :: AlexPosn, valueC :: [Int]}
--     | TokenINT         {position :: AlexPosn}
--     | TokenCOLL        {position :: AlexPosn}
--     | TokenBOOL        {position :: AlexPosn}
--     | TokenTInt        {position :: AlexPosn}
--     | TokenTBool       {position :: AlexPosn}
--     | TokenTColl       {position :: AlexPosn}
--     | TokenVar         {position :: AlexPosn}
--     | TokenExpr        {position :: AlexPosn}
       
