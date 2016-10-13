{-# LANGUAGE NoImplicitPrelude #-}

module MinCaml.Syntax.Type where

import           Prelude    (IO, String, error, (<$>))

import           Data.IORef (IORef, newIORef)
import           Data.Maybe (Maybe (..))

data Type
  = Unit
  | Bool
  | Int
  | Float
  | Fun [Type] Type
  | Tuple [Type]
  | Array Type
  | Var (IORef (Maybe Type))

genType :: IO Type
genType = Var <$> newIORef Nothing
