module Id where

import           MinCaml.Syntax.Type

import           Control.Monad.IO.Class (MonadIO)
import           Data.IORef

data Id = Id String

class HasCounter m where
  getC :: m Int
  modC :: (Int -> Int) -> m ()

incC :: MonadIO m => m Int
incC = modC (+1) >> getC

decC :: MonadIO m => m Int
decC = modC (subtract 1) >> getC

genId :: (MonadIO m, HasCounter m) => String -> m Id
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

genTmp :: (MonadIO m, HasCounter m) => Type -> m Id
genTmp typ = Id . printf "T%s%d" (typeId typ) <$> incC
