{
module Lexer where
}

%wrapper "basic"

$white     = [\ \t\n\r]
$digit     = [0-9]
$alpha     = [A-Za-z]
$alphanum  = [$alpha $digit]

tokens :-

$white+    ; -- ignorar espaços em branco

"fun"           { \_ -> FUN }
"println"       { \_ -> PRINTLN }
"readln"        { \_ -> READLN }
"print"         { \_ -> PRINT }
"read"          { \_ -> READ }
"val"           { \_ -> VAL }
"var"           { \_ -> VAR }
"if"            { \_ -> IF }
"else"          { \_ -> ELSE }
"when"          { \_ -> WHEN }
"for"           { \_ -> FOR }
"while"         { \_ -> WHILE }
"do"            { \_ -> DO }
"return"        { \_ -> RETURN }
"class"         { \_ -> CLASS }
"object"        { \_ -> OBJECT }
"open"          { \_ -> OPEN }
"is"            { \_ -> IS }
"in"            { \_ -> IN }
"null"          { \_ -> NULL }

"+"  { \_ -> PLUS }
"-"  { \_ -> MINUS }
"*"  { \_ -> MULT }
"/"  { \_ -> DIV }
".." { \_ -> RANGETO }
"..<"{ \_ -> RANGEUNTIL }
"="  { \_ -> LEQ }
"==" { \_ -> LEQEQ }
"!=" { \_ -> LNEQ }
">"  { \_ -> LGT }
"<"  { \_ -> LLT }
">=" { \_ -> LGTEQ }
"<=" { \_ -> LLTEQ }
"||" { \_ -> OR }
"&&" { \_ -> AND }
"!"  { \_ -> NOT }
"true" { \_ -> TRUE }
"false" { \_ -> FALSE }
"(" { \_ -> LPAREN }
")" { \_ -> RPAREN }
"{" { \_ -> LBRACE }
"}" { \_ -> RBRACE }
"[" { \_ -> LBRACKET }
"]" { \_ -> RBRACKET }
"," { \_ -> COMMA }
";" { \_ -> SEMICOLON }
":" { \_ -> COLON }

$digit+ { \s -> NUM (read s) } 
$digit+"."$digit+ { \s -> REAL (read s) }
$alpha($alphanum)*  { \s -> ID s }
(("//".*)) { \_ -> COMMENT }
("/*"([^\*]|(\*+[^\*\/]))*"*/" ) { \_ -> COMMENT }
\".*\" { \s -> FRASE s } -- Corrigido para associar a frase corretamente

{
-- Definição dos tokens
data Token
  = PLUS
  | MINUS
  | MULT
  | DIV
  | FUN
  | PRINTLN
  | READLN
  | PRINT
  | READ
  | VAL
  | VAR
  | IF
  | ELSE
  | WHEN
  | FOR
  | WHILE
  | DO
  | RETURN
  | CLASS
  | OBJECT
  | OPEN
  | IS
  | IN
  | NULL
  | RANGETO
  | RANGEUNTIL
  | LEQ   -- Mudando EQ para LEQ
  | LEQEQ -- Mudando EQEQ para LEQEQ
  | LNEQ  -- Mudando NEQ para LNEQ
  | LGT   -- Mudando GT para LGT
  | LLT   -- Mudando LT para LLT
  | LGTEQ -- Mudando GTEQ para LGTEQ
  | LLTEQ -- Mudando LTEQ para LLTEQ
  | OR
  | AND
  | NOT
  | TRUE
  | FALSE
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COMMA
  | SEMICOLON
  | COLON
  | NUM Int
  | REAL Double
  | ID String
  | COMMENT 
  | FRASE String -- Adiciona o tipo String para FRASE
  deriving (Eq, Show)
}

