module Sofia.Token where


data Token
  = NameToken String
  | CharLiteralToken Char
  | IntLiteralToken Integer
  | DoubleLiteralToken Double
  | LetToken            -- let
  | MutToken            -- mut
  | IfToken             -- if
  | ElseToken           -- else
  | ForToken            -- for
  | WhileToken          -- while
  | FunctionToken       -- function
  | ReturnToken         -- return
  | UndefinedToken      -- undefined
  | LParenToken         -- (
  | RParenToken         -- )
  | LBraceToken         -- {
  | RBraceToken         -- }
  | CommaToken          -- ,
  | SemiToken           -- ;
  | EqToken             -- =
  deriving (Eq, Show, Read)
