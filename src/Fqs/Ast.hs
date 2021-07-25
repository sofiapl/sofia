module Fqs.Ast where

import Data.HashSet (HashSet (..))
import Data.Hashable

import qualified Sofia.Ast as Sofia


-- подумать над реестром имён
data Program = Program
  { getGlobalLets         :: [(Integer, Expr)]
  , getFunctionsRegistry  :: [(Integer, Function)]
  , getLetsRegistry       :: [(Integer, Let)]
  } deriving Show

data Let = Let
  { getLetName    :: String
  , getLetType    :: Type
  , isLetMutable  :: Bool
  } deriving Show

data Function = Function
  { getFunctionName :: Maybe String
  , getFunctionArgs :: [(String, Type)]
  , getFunctionType :: Type
  , getFunctionBody :: [Stmt]
  } deriving Show

data Type
  = UnionType (HashSet Value)
  deriving (Eq, Show)

data Stmt
  = ExprStmt Expr
  | ReturnStmt Expr
  | LetStmt Integer Type Expr
  | IfStmt Expr [Stmt]
  | IfElseStmt Expr [Stmt] [Stmt]
  deriving Show

data Expr
  = NameExpr Integer
  | ValueExpr Value
  | SetExpr Integer Expr
  | CallExpr Integer [Expr]
  deriving Show

data Value
  = UnitValue
  | IntegerValue Integer
  | DoubleValue Double
  | BoolValue Bool
  | CharValue Char
  | FunctionValue (Maybe String) [String] [(String, Integer)] [Sofia.Stmt]
  deriving (Eq, Show)

instance Hashable Value where
  hashWithSalt s UnitValue = 0 + 2 * hashWithSalt s ()
  hashWithSalt s (IntegerValue v) = 1 + 2 * hashWithSalt s v
  hashWithSalt s (DoubleValue v) = 2 + 2 * hashWithSalt s v
  hashWithSalt s (BoolValue v) = 3 + 2 * hashWithSalt s v
  hashWithSalt s (CharValue v) = 4 + 2 * hashWithSalt s v
  hashWithSalt s (FunctionValue n p c b) = 5 + 2 * hashWithSalt s n + 4 * hashWithSalt s p
    + 8 * hashWithSalt s c + 16 * hashWithSalt s (show b)

emptyProgram :: Program
emptyProgram = Program [] [] []

addFunction
  :: Function
  -> Program
  -> (Integer, Program)
addFunction f p = case getFunctionsRegistry p of
  [] -> (0, p { getFunctionsRegistry = [(0, f)] })
  r@((i, _):_) -> (i + 1, p { getFunctionsRegistry = (i + 1, f):r })

getFunction
  :: Integer
  -> Program
  -> Function
getFunction i (Program { getFunctionsRegistry = fs }) = case filter ((i ==) . fst) fs of
  [] -> error $ "no function with id " ++ show i ++ " found"
  (_, f):_ -> f

addLet
  :: Let
  -> Program
  -> (Integer, Program)
addLet l p = case getLetsRegistry p of
  [] -> (0, p { getLetsRegistry = [(0, l)] })
  r@((i, _):_) -> (i + 1, p { getLetsRegistry = (i + 1, l):r })

getLet
  :: Integer
  -> Program
  -> Let
getLet i (Program { getLetsRegistry = lets }) = case filter ((i ==) . fst) lets of
  [] -> error $ "no let with id " ++ show i ++ " found"
  (_, l):_ -> l

setLetType
  :: Integer
  -> Type
  -> Program
  -> Program
setLetType i t p = case setLetType' (getLetsRegistry p) of
    (True, lets) -> p { getLetsRegistry = lets }
    (False, _) -> error $ "no let with id " ++ show i ++ " found"
  where
    setLetType' :: [(Integer, Let)] -> (Bool, [(Integer, Let)])
    setLetType' [] = (False, [])
    setLetType' ((i', l):ls) | i' == i = (True, (i, l { getLetType = t }):ls)
    setLetType' (l:ls) = let
      (r, ls') = setLetType' ls
      in (r, l:ls')

addGlobalLet
  :: (Integer, Expr)
  -> Program
  -> Program
addGlobalLet gl p = p { getGlobalLets = (gl:getGlobalLets p) }
