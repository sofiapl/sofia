module Type where

import Union


data Type
  = NonameType Union
  | NamedType String Union
