module Main where

import Data.Maybe (fromJust)
import Data.HashSet (singleton)
import Debug.Trace

import Sofia.Lexer (lexSofia)
import Sofia.Parser (parseSofia)
import Fqs.Ast as Fqs
import SofiaToFqs
import Utils.ErrorTree


main :: IO ()
main = do
  code <- readFile "test.sf"

  putStrLn "Token list:"
  case lexSofia code of
    Left err -> putStrLn err
    Right tokenList -> do
      mapM_ putStrLn $ map show tokenList

      putStrLn "AST:"
      case parseSofia tokenList of
        Left err -> putStrLn err
        Right ast -> do
          putStrLn $ show ast

          putStrLn "Fully qualified:"
          case sofiaToFqs ast "main" [] [Fqs.UnionType $ singleton $ Fqs.UnitValue] of
            Left err -> printErrorTree err
            Right fqs -> putStrLn $ show fqs

-- interpret :: Program -> IO ()
-- interpret program =
--   let
--     functionList :: [(String, ([String], [Stmt]))]
--     functionList = map (\(FunctionStmt name args body) -> (name, (args, body)))
--       $ filter (\s -> case s of FunctionStmt _ _ _ -> True ; _ -> False) program
--
--     globalScope :: [(String, Value)]
--     globalScope = map (\s -> case s of
--         FunctionStmt name args _ -> (name, FunctionValue [] args
--           [ReturnStmt $ CallExpr (NameExpr name) $ map NameExpr args])
--       ) program
--
--     callFunction
--       :: String
--       -> [Value]
--       -> [(String, ([String], [Stmt]))]
--       -> Either String Value
--     callFunction name args fs = case map snd $ filter ((== name) . fst) fs of
--       [] -> Left $ "there are not a function named " ++ name
--       fs -> case trace (name ++ "(" ++ show args ++ ")") $ resolveAndCallFunction args fs of
--         Left err -> Left $ err ++ " named " ++ name
--         result   -> result
--
--     resolveAndCallFunction
--       :: [Value]
--       -> [([String], [Stmt])]
--       -> Either String Value
--     resolveAndCallFunction args fs = tryCallFunctions args
--       $ filter ((== length args) . length . fst) fs
--
--     tryCallFunctions
--       :: [Value]
--       -> [([String], [Stmt])]
--       -> Either String Value
--     tryCallFunctions args fs = case map fromJust $ filter (/= Nothing)
--       $ map (callFunction' args globalScope) fs of
--         []  -> Left $ "there are not defined functions for args " ++ show args
--         [v] -> Right v
--         _   -> Left $ "there are more than one defined functions for args " ++ show args
--
--     callFunction'
--       :: [Value]
--       -> [(String, Value)]
--       -> ([String], [Stmt])
--       -> Maybe Value
--     callFunction' args scope (argNames, stmts) = execStmts (zip argNames args ++ scope) stmts
--
--     execStmts :: [(String, Value)] -> [Stmt] -> Maybe Value
--     execStmts scope []           = Just UnitValue
--     execStmts scope (stmt:stmts) = case stmt of
--       ExprStmt expr -> case evalExpr scope expr of
--         Nothing -> trace ("undefined in expr " ++ show expr) Nothing
--         _       -> execStmts scope stmts
--       ReturnStmt expr -> evalExpr scope expr
--
--     evalExpr :: [(String, Value)] -> Expr -> Maybe Value
--     evalExpr scope expr = case expr of
--       NameExpr name -> lookup name scope
--       UndefinedExpr -> Nothing
--       CallExpr f args -> let
--           argsValues = map (evalExpr scope) args
--           argsValues' = map fromJust argsValues
--         in if (length $ filter (/= Nothing) argsValues) == length args
--           then case evalExpr scope f of
--             Just (FunctionValue scope fArgs body) -> callFunction' argsValues' scope (fArgs, body)
--             Just _ -> trace ("not a function in expr " ++ show f) Nothing
--             Nothing -> case f of
--               NameExpr name -> case callFunction name argsValues' functionList of
--                 Right v  -> Just v
--                 Left err -> trace ("error in expr " ++ show expr ++ ": " ++ err) Nothing
--               _ -> trace ("not a name expr " ++ show f) Nothing
--           else trace ("there are undefined values in args " ++ show argsValues) Nothing
--       FunctionExpr args body -> Just $ FunctionValue scope args body
--
--     callMainFunction :: IO ()
--     callMainFunction = case callFunction "main" [] functionList of
--       Left err -> putStrLn err
--       _        -> return ()
--
--   in callMainFunction
