module KTP.AST where

import Data.Map (Map)
import qualified Data.Map as M

data Expr = Z
          | S
          | I Int Integer
          | C Fun [Fun]
          | P Fun Fun
          | M Fun
          | FC FunName
          deriving (Eq, Show)

type FunName = String
type Fun = Expr
type Program = Map String Expr
