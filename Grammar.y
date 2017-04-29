{
module Grammar where
import Tokens
}

%name expCalc
%tokentype { Token }
%error { parseError }

%token
    int     { TokenInt $$ }
    double  { TokenDouble $$ }
    var     { TokenSym $$ }
    '='     { TokenAssign }
    '+'     { TokenPlus }
    '-'     { TokenMinus }
    '*'     { TokenTimes }
    '/'     { TokenDiv }
    '('     { TokenLParen }
    ')'     { TokenRParen }

%right '='
%left '+' '-'
%left '*' '/'
%left NEG

%%

Exp : var '=' Exp           { Assign $1 $3 }
    | Exp '+' Exp           { Plus $1 $3 }
    | Exp '-' Exp           { Minus $1 $3 }
    | Exp '*' Exp           { Times $1 $3 }
    | Exp '/' Exp           { Div $1 $3 }
    | '(' Exp ')'           { $2 }
    | '-' Exp %prec NEG     { Negate $2 }
    | int                   { Int $1 }
    | double                { Double $1 }
    | var                   { Var $1 }

{

parseError :: [Token] -> a
parseError _= error "Parse Error"

data Exp = Assign String Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Negate Exp
         | Int Int
         | Double Double
         | Var String
         deriving Show
}
