{-# LANGUAGE NoImplicitPrelude #-}

module Syntax.Term where

import qualified Prelude     as P

import           Syntax.Id   (Id)
import           Syntax.Type (Type)

data Term
  = Unit
  | Bool P.Bool
  | Int P.Int
  | Float P.Float
  | Not Term
  | Neg Term
  | Add Term Term
  | Sub Term Term
  | FNeg Term
  | FAdd Term Term
  | FSub Term Term
  | FMul Term Term
  | FDiv Term Term
  | Eq Term Term
  | LE Term Term
  | If Term Term Term
  | Let (Id, Type) Term Term
  | Var Id
  | LetRec FunDef Term
  | App Term [Term]
  | Tuple [Term]
  | LetTuple [(Id, Type)] Term Term
  | Array Term Term
  | Get Term Term
  | Put Term Term Term

data FunDef
  = FunDef
  { name :: Id Type
  , args :: [(Id, Type)]
  , body :: Term
  }
