{
{-# LANGUAGE MultiParamTypeClasses #-}

module MinCaml.Lexer where

import MinCaml.Syntax.Id
import MinCaml.Syntax.Type (Type (Unit))

import Control.Monad.State
}

%wrapper "monadUserState"

$space = [ \t\n\r]
$digit = [0-9]
$lower = [a-z]
$upper = [A-Z]

tokens :-

<0>              $space+ { mkTok TWspc }
<0>              "(*"    { enterComment `andBegin` state_comment }
<state_comment>  "*)"    { leaveComment }
<state_comment>  "(*"    { enterComment }
<state_comment>  eof     { error "warning: unterminated comment@." }
<state_comment>  .       ;
<state_comment>  \n      { skip }
<0>              "("     { mkTok TLparen }
<0>              ")"     { mkTok TRparen }
<0>              "true"  { mkTok (TBool True) }
<0>              "false" { mkTok (TBool False) }
<0>              "not"   { mkTok TNot }
<0>              $digit+ { mkTokStr (TInt . read) }
<0>              $digit+(.$digit+)?([eE][\-\+]?$digit+)? { mkTokStr (TFloat . read) }
<0>              "-"     { mkTok TMinus }
<0>              "+"     { mkTok TPlus }
<0>              "-."    { mkTok TMinusDot }
<0>              "+."    { mkTok TPlusDot }
<0>              "*."    { mkTok TAstDot }
<0>              "/."    { mkTok TSlashDot }
<0>              "="     { mkTok TEqual }
<0>              "<>"    { mkTok TLessGreater }
<0>              "<="    { mkTok TLessEqual }
<0>              ">="    { mkTok TGreaterEqual }
<0>              "<"     { mkTok TLess }
<0>              ">"     { mkTok TGreater }
<0>              "if"    { mkTok TIf }
<0>              "then"  { mkTok TThen }
<0>              "else"  { mkTok TElse }
<0>              "let"   { mkTok TLet }
<0>              "in"    { mkTok TIn }
<0>              "rec"   { mkTok TRec }
<0>              ","     { mkTok TComma }
<0>              "_"     { newIdFromCounter }
<0>              "Array.create" { mkTok TArrayCreate }
<0>              "."     { mkTok TDot }
<0>              "<-"    { mkTok TLessMinus }
<0>              ";"     { mkTok TSemicolon }
<0>              eof     { mkTok TEOF }
<0>              $lower ($digit | $lower | $upper | '_')* { mkTokStr (TIdent . Id) }

{
data Token
  = Token TokenClass AlexPosn String
  deriving Show
data TokenClass
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
  deriving (Eq, Show)

mkTok :: TokenClass -> AlexInput -> Int -> Alex Token
mkTok c (p, _, _, str) len = return $ Token c p (take len str)

mkTokStr :: (String -> TokenClass) -> AlexInput -> Int -> Alex Token
mkTokStr c (p, _, _, str) len = return $ Token (c s) p s
  where s = take len str

newIdFromCounter :: AlexInput -> Int -> Alex Token
newIdFromCounter inp len = do
  newId <- genTmp Unit
  mkTok (TIdent newId) inp len

data AlexUserState
  = AlexUserState
  { counter :: Int
  , depth :: Int
  }

alexEOF = return (Token TEOF undefined "")

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0 0

modUst :: (AlexUserState -> AlexUserState) -> AlexState -> AlexState
modUst f st@AlexState { alex_ust = ust } = st { alex_ust = f ust }

modDepth :: (Int -> Int) -> AlexState -> AlexState
modDepth f = modUst (\ (AlexUserState c d) -> AlexUserState c (f d))

modCount :: (Int -> Int) -> AlexState -> AlexState
modCount f = modUst (\ (AlexUserState c d) -> AlexUserState (f c) d)

incDepth, decDepth :: AlexState -> AlexState
incDepth = modDepth (+1)
decDepth = modDepth (subtract 1)

getDepth :: Alex Int
getDepth = depth . alex_ust <$> get

enterComment :: AlexInput -> Int -> Alex Token
enterComment inp len = do
  get >>= put . incDepth
  skip inp len

leaveComment :: AlexInput -> Int -> Alex Token
leaveComment inp len = do
  get >>= put . decDepth
  d <- getDepth
  begin (if d == 0 then 0 else state_comment) inp len

instance HasCounter Alex where
  getC = counter . alex_ust <$> get
  modC f = get >>= put . modCount f

instance MonadState AlexState Alex where
  get = Alex $ \st -> Right (st, st)
  put st = Alex $ \st -> Right (st, ())
}
