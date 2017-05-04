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
    ','     { TokenComma }
    '='     { TokenAssign }
    '+'     { TokenPlus }
    '-'     { TokenMinus }
    '*'     { TokenTimes }
    '/'     { TokenDiv }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    '['     { TokenLBracket }
    ']'     { TokenRBracket }
    ';'     { TokenSemicolon }

%right '='
%left '+' '-'
%left '*' '/'
%left NEG

%%

Stmnts : Stmnt              { [$1] }
       | Stmnts Stmnt       { $2 : $1 }

Stmnt : Exp ';'             { Stmnt $1 }
      | var '=' Exp ';'     { StmntAssign $1 $3 }

Exp : var '(' args ')'      { Call $1 $3 }
    | Exp '+' Exp           { Plus $1 $3 }
    | Exp '-' Exp           { Minus $1 $3 }
    | Exp '*' Exp           { Times $1 $3 }
    | Exp '/' Exp           { Div $1 $3 }
    | '(' Exp ')'           { $2 }
    | '-' Exp %prec NEG     { Negate $2 }
    | int                   { Int $1 }
    | double                { Double $1 }
    | Array                 { $1 }
    | var                   { Var $1 }

Array : '[' args ']'        { Array $2 }

args : {- empty -}          { [] }
     | Exp                  { [$1] }
     | args ',' Exp         { $3 : $1 }

{

parseError :: [Token] -> a
parseError a = error "Parse Error"

data Stmnt = Stmnt Exp
          | StmntAssign String Exp

data Exp = Assign String Exp
         | Call String [Exp]
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Negate Exp
         | Int Int
         | Double Double
         | Array [Exp]
         | Var String
         deriving Show
}
