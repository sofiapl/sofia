module Sofia.Ast where


type Program = [TopStmt]

data TopStmt
  = FunctionStmt String [String] [Stmt]
  | GlobalLetStmt String Expr
  deriving (Eq, Show)

data Stmt
  = ExprStmt Expr
  | ReturnStmt Expr
  | LetStmt String Expr
  | LetMutStmt String Expr
  | IfStmt Expr [Stmt]
  | IfElseStmt Expr [Stmt] [Stmt]
  deriving (Eq, Show)

data Expr
  = NameExpr String
  | CharLiteralExpr Char
  | IntLiteralExpr Integer
  | DoubleLiteralExpr Double
  | UndefinedExpr
  | SetExpr String Expr
--  | IfExpr Expr [Stmt] [Stmt]
  | CallExpr Expr [Expr]
  | FunctionExpr [String] [Stmt]
  deriving (Eq, Show)
