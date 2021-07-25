module Value where

import Data.Hashable

import RealNum
import Ast


-- There isn't undefined value because it is not a valid value
data Value
  = UnitValue
  | IntegerValue Integer
  | RealValue RealNum
  | BoolValue Bool
  | CharValue Char
  | StringValue String
  | ArrayValue [Value]
  | FunctionValue [(String, Value)] [String] [Stmt]
  deriving (Eq, Show)

instance Hashable Value where
  hashWithSalt s (IntegerValue v)  = hashWithSalt s v
  hashWithSalt s (RealValue v)     = hashWithSalt s v
  hashWithSalt s (CharValue v)     = hashWithSalt s v
  hashWithSalt s (StringValue v)   = hashWithSalt s v
  hashWithSalt s (ArrayValue v)    = hashWithSalt s v
-- TODO
  hashWithSalt s (FunctionValue _ _ _) = 0
