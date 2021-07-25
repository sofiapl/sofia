module Union where

import Data.HashSet

import Value
import Utils.RealNum


data Union
  = ValueUnion        (HashSet Value)
  | RealUnion         (Maybe RealNum) (Maybe RealNum) (Maybe RealNum)
  | IntegerUnion      (Maybe Integer) (Maybe Integer)  Integer
  | UnionUnion        Union Union
  | IntersectionUnion Union Union
  | DifferenceUnion   Union Union
  | NegationUnion     Union

union :: Union -> Union -> Union
union (ValueUnion a) (ValueUnion b) = ValueUnion $ Data.HashSet.union a b
-- TODO more union reduction cases
union a b = UnionUnion a b


intersection :: Union -> Union -> Union
intersection (ValueUnion a) (ValueUnion b) = ValueUnion $ Data.HashSet.intersection a b
-- TODO more intersection reduction cases
intersection a b = IntersectionUnion a b


difference :: Union -> Union -> Union
difference (ValueUnion a) (ValueUnion b) = ValueUnion $ Data.HashSet.difference a b
-- TODO more difference reduction cases
difference a b = DifferenceUnion a b

negation :: Union -> Union
-- TODO negation reduction cases
negation = NegationUnion
