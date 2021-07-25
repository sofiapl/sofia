module Value where

import Data.Hashable

import Utils.RealNum
import Sofia.Ast


-- There isn't undefined value because it is not a valid value
-- TODO replace BoolValue to custom type
data Value
  = UnitValue
  | IntegerValue Integer
  | RealValue RealNum
  | BoolValue Bool
  | CharValue Char
--   | FunctionValue [(String, Value)] [String] [Stmt] -- TODO
  deriving (Eq, Show)

instance Hashable Value where
  hashWithSalt s  UnitValue         = hashWithSalt s ()
  hashWithSalt s (IntegerValue v)   = hashWithSalt s v
  hashWithSalt s (RealValue v)      = hashWithSalt s v
  hashWithSalt s (CharValue v)      = hashWithSalt s v
