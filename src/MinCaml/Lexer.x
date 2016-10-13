{
module MinCaml.Lexer where

import MinCaml.Syntax.Id (genTmp)
import MinCaml.Syntax.Type (Type (Unit))
}

%wrapper "monadUserState"

$space = [ \t\n\r]
$digit = [0-9]
$lower = [a-z]
$upper = [A-Z]

tokens :-
<0>              $space+ { TWspc }
<0>              "(\*" { decDepth `andBegin` state_comment }
<state_comment>  "*)" { decDepth ; decC >>= \x -> when (x == 0) (begin 0) }
<state_comment>  "(*" { incDepth }
<state_comment>  eof { putStrLn "warning: unterminated comment@." }
<state_comment>  . ;
<state_comment>  \n { skip }
<0>              "(" { TLparen }
<0>              ")" { TRparen }
<0>              "true" { TBool True }
<0>              "false" { TBool False }
<0>              "not" { TNot }
<0>              $digit+ { TInt . read }
<0>              $digit+(.$digit+)?([eE][\-\+]?$digit+)? { TFloat . read }
<0>              "-" { TMinus }
<0>              "+" { TPlus }
<0>              "-." { TMinusDot }
<0>              "+." { TPlusDot }
<0>              "*." { TAstDot }
<0>              "/." { TSlashDot }
<0>              "=" { TEqual }
<0>              "<>" { TLessGreater }
<0>              "<=" { TLessEqual }
<0>              ">=" { TGreaterEqual }
<0>              "<" { TLess }
<0>              ">" { TGreater }
<0>              "if" { TIf }
<0>              "then" { TThen }
<0>              "else" { TElse }
<0>              "let" { TLet }
<0>              "in" { TIn }
<0>              "rec" { TRec }
<0>              "," { TComma }
<0>              "_" { TIdent (genTmp Unit) }
<0>              "Array.create" { TArrayCreate }
<0>              "." { TDot }
<0>              "<-" { TLessMinus }
<0>              ";" { TSemicolon }
<0>              eof { TEOF }
<0>              $lower ($digit | $lower | $upper | '_')* { TIdent . Id }

{
data Token
  = TBool Bool
  | TInt Int
  | TFloat Float
  | TNot
  | TMinus
  | TPlus
  | TMinusDot
  | TPlusDot
  | TAstDot
  | TSlashDot
  | TEqual
  | TLessGreater
  | TLessEqual
  | TGreaterEqual
  | TLess
  | TGreater
  | TIf
  | TThen
  | TElse
  | TIdent Id
  | TLet
  | TIn
  | TRec
  | TComma
  | TArrayCreate
  | TDot
  | TLessMinus
  | TSemicolon
  | TLparen
  | TRparen
  | TEOF
  -- extras
  | TWspc
  | TComment
  deriving (Eq, Show)

data AlexUserState
  = AlexUserState
  { counter :: Int
  , depth :: Int
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0 0

modUst :: (AlexUserState -> AlexUserState) -> AlexState -> AlexState
modUst f st@AlexState { alex_ust = ust } = st { alex_ust = f ust }

modDepth = modUst (\(AlexUserState c d) -> AlexUserState c (f d))
incDepth = modDepth (+1)
decDepth = modDepth (subtract 1)

instance HasCounter AlexState where
  getC st = counter . alex_ust
  modC f = modUst (\(AlexUserState c d) -> AlexUserState (f c) d)
}
