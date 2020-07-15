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
    "(<" $digit* ")"                    { \pos str -> TokenFopGt pos (read str)}

{
data Token = TokenLBrak       {pos :: AlexPosn} -- [
           | TokenRBrak       {pos :: AlexPosn} -- ]
           | TokenLet         {pos :: AlexPosn} -- :=
           | TokenAdd         {pos :: AlexPosn} -- +
           | TokenMinus       {pos :: AlexPosn} -- -
           | TokenTimes       {pos :: AlexPosn} -- *
           | TokenDiv         {pos :: AlexPosn} -- /
           | TokenMod         {pos :: AlexPosn} -- %
           | TokenSgn         {pos :: AlexPosn} -- ~
           | TokenIndep       {pos :: AlexPosn} -- #
           | TokenOpen        {pos :: AlexPosn} -- (
           | TokenClose       {pos :: AlexPosn} -- )
           | TokenComma       {pos :: AlexPosn} -- ,
           | TokenFopGt       {pos :: AlexPosn, value :: Int}
           | TokenFopLt       {pos :: AlexPosn, value :: Int}
           | TokenFopGEt      {pos :: AlexPosn, value :: Int}
           | TokenFopLEt      {pos :: AlexPosn, value :: Int}
           | TokenFopEq       {pos :: AlexPosn, value :: Int}
           | TokenFopNEq      {pos :: AlexPosn, value :: Int}
           | TokenD           {pos :: AlexPosn}
           | TokenZ           {pos :: AlexPosn}
           | TokenLeast       {pos :: AlexPosn}
           | TokenLargt       {pos :: AlexPosn}
           | TokenFilter      {pos :: AlexPosn}
           | TokenConcat      {pos :: AlexPosn} -- @@
           | TokenMax         {pos :: AlexPosn}
           | TokenMin         {pos :: AlexPosn}
           | TokenSum         {pos :: AlexPosn}
           | TokenCount       {pos :: AlexPosn}
           | TokenLt          {pos :: AlexPosn} -- <
           | TokenGt          {pos :: AlexPosn} -- >
           | TokenGEt         {pos :: AlexPosn} -- >=
           | TokenLEt         {pos :: AlexPosn} -- <=
           | TokenEq          {pos :: AlexPosn} -- ==
           | TokenNEq         {pos :: AlexPosn} -- /=
           | TokenAnd         {pos :: AlexPosn} -- &&
           | TokenOr          {pos :: AlexPosn} -- ||
           | TokenNot         {pos :: AlexPosn} -- ¬
           | TokenIsEmpty     {pos :: AlexPosn}
           | TokenSeq         {pos :: AlexPosn} -- ;
           | TokenIf          {pos :: AlexPosn}
           | TokenThen        {pos :: AlexPosn}
           | TokenElse        {pos :: AlexPosn}
           | TokenAccum       {pos :: AlexPosn}
           | TokenRepeat      {pos :: AlexPosn}
           | TokenUntil       {pos :: AlexPosn}
           | TokenTrue        {pos :: AlexPosn}
           | TokenFalse       {pos :: AlexPosn}
           | TokenInt         {pos :: AlexPosn, value :: Int}
           | TokenName        {pos :: AlexPosn, text :: String}
 deriving (Eq, Show)



tokenPos :: Token -> (Int, Int)
tokenPos t = let (AlexPn v x y) = (pos t) in (x,y)

lexer = alexScanTokens
}

--     Tokens i'll probably never use
--     | TokenCollection  {pos :: AlexPosn, valueC :: [Int]}
--     | TokenINT         {pos :: AlexPosn}
--     | TokenCOLL        {pos :: AlexPosn}
--     | TokenBOOL        {pos :: AlexPosn}
--     | TokenTInt        {pos :: AlexPosn}
--     | TokenTBool       {pos :: AlexPosn}
--     | TokenTColl       {pos :: AlexPosn}
--     | TokenVar         {pos :: AlexPosn}
--     | TokenExpr        {pos :: AlexPosn}

-- However i should make these ones
--    $lower [$alpha $digit \_ \']*       { \pos str -> TokenFopGt pos str}
--    $lower [$alpha $digit \_ \']*       { \pos str -> TokenFopLt pos str}
--    $lower [$alpha $digit \_ \']*       { \pos str -> TokenFopGEt pos str}
--    $lower [$alpha $digit \_ \']*       { \pos str -> TokenFop pos str}

       
