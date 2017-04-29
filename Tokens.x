{
  module Tokens where
}

%wrapper "basic"


tokens :-
  $white+                                     ;
  [0-9]+                                      { \s -> TokenInt (read s) }
  [0-9]+\.[0-9]+                              { \s -> TokenDouble (read s) }
  \,                                          { \_ -> TokenComma }
  \=                                          { \_ -> TokenAssign }
  \+                                          { \_ -> TokenPlus }
  \-                                          { \_ -> TokenMinus }
  \*                                          { \_ -> TokenTimes }
  \/                                          { \_ -> TokenDiv }
  \(                                          { \_ -> TokenLParen }
  \)                                          { \_ -> TokenRParen }
  [a-zA-Z\_][a-zA-Z0-9\_]*                    { \s -> TokenSym s }

{

data Token = TokenInt Int
           | TokenDouble Double
           | TokenComma
           | TokenAssign
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLParen
           | TokenRParen
           | TokenSym String
           deriving (Eq, Show)

scanTokens = alexScanTokens

}
