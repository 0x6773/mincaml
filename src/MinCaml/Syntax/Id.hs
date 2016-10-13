module MinCaml.Syntax.Id where

import           MinCaml.Syntax.Type

import           Text.Printf

data Id
  = Id String
  deriving (Show, Eq)

class HasCounter m where
  getC :: m Int
  modC :: (Int -> Int) -> m ()

incC :: (Monad m, HasCounter m) => m Int
incC = modC (+1) >> getC

decC :: (Monad m, HasCounter m) => m Int
decC = modC (subtract 1) >> getC

genId :: (Monad m, HasCounter m) => String -> m Id
genId s = Id . printf "%s.%d" s <$> incC

typeId :: Type -> String
typeId c = case c of
  Unit    -> "u"
  Bool    -> "b"
  Int     -> "i"
  Float   -> "d"
  Fun _ _ -> "f"
  Tuple _ -> "t"
  Array _ -> "a"
  Var _   -> error "assert False"

genTmp :: (Monad m, HasCounter m) => Type -> m Id
genTmp typ = Id . printf "T%s%d" (typeId typ) <$> incC
