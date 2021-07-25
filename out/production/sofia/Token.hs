module Token where


data Token
  = NameToken String
  | FunctionToken       -- function
  | ReturnToken         -- return
  | UndefinedToken      -- undefined
  | LParenToken         -- (
  | RParenToken         -- )
  | LBraceToken         -- {
  | RBraceToken         -- }
  | ColonToken          -- ,
  | SemiToken           -- ;
  deriving (Eq, Show, Read)
