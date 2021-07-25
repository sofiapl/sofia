module Ast where


type Program = [TopStmt]

data TopStmt
  = FunctionStmt String [String] [Stmt]
  | GlobalLetStmt String Expr
  deriving (Eq, Show, Read)

data Stmt
  = ExprStmt Expr
  | ReturnStmt Expr
  deriving (Eq, Show, Read)

data Expr
  = NameExpr String
  | UndefinedExpr
  | CallExpr Expr [Expr]
  | FunctionExpr [String] [Stmt]
  deriving (Eq, Show, Read)
