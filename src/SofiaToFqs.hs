module SofiaToFqs where

import Data.Tuple (swap)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.List (foldl', nubBy)
import Data.Either (isRight, fromLeft, fromRight)
import Data.HashSet (singleton, toList)
import Control.Monad (foldM)

import Utils.ErrorTree
import qualified Sofia.Ast as Sofia
import qualified Fqs.Ast as Fqs


sofiaToFqs
  :: Sofia.Program                -- sofia program
  -> String                       -- entry point name
  -> [Fqs.Type]                   -- entry point parameters
  -> [Fqs.Type]                   -- list of valid return types of entry point function
  -> Either ErrorTree Fqs.Program -- fully qualified sofia program
sofiaToFqs s n p r = case filter (flip elem r . snd) <$> processEntryPoint s n p of
  Right [(p, _)] -> Right p
  Right [] -> Left $ ErrorTree "there isn't any entry point with valid return type" []
  Right es -> Left $ ErrorTree ("ambigious entry point function definition: " ++ show es) []
  Left err -> Left err

processEntryPoint
  :: Sofia.Program                              -- sofia program
  -> String                                     -- entry point name
  -> [Fqs.Type]                                 -- entry point parameters
  -> Either ErrorTree [(Fqs.Program, Fqs.Type)] -- list of FQS programs with return types of entry point
processEntryPoint s n p = let
  results = processGlobalFunction s n p Fqs.emptyProgram
  in case filter isRight results of
    []  -> Left $ ErrorTree "there isn't entry point function" $ map (fromLeft undefined) results
    rs  -> Right $ map (fromRight undefined) rs

processGlobalFunction
  :: Sofia.Program                              -- sofia program
  -> String                                     -- global function name
  -> [Fqs.Type]                                 -- parameters
  -> Fqs.Program                                -- partial FQS program
  -> [Either ErrorTree (Fqs.Program, Fqs.Type)] -- list of FQS programs with return types of function
processGlobalFunction s n p r = fmap f'
  $ findFunctionDefinitions n s >>= processFunctionDefinition'
  where
    processFunctionDefinition'
      :: ([String], [Sofia.Stmt])
      -> [Either ErrorTree (Fqs.Program, Integer)]
    processFunctionDefinition' (fp, fb) = processFunctionDefinition s (Just n) p r (fp, [], fb)

    f' (Left err) = Left err
    f' (Right (r', id)) = case lookup id $ Fqs.getFunctionsRegistry r' of
      Just f -> Right (r', Fqs.getFunctionType f)

findFunctionDefinitions
  :: String                     -- global function name
  -> Sofia.Program              -- sofia program
  -> [([String], [Sofia.Stmt])] -- list of (parameter names, function body)
findFunctionDefinitions name = map m . filter flt where
  flt :: Sofia.TopStmt -> Bool
  flt (Sofia.FunctionStmt n _ _) = n == name
  flt _ = False

  m :: Sofia.TopStmt -> ([String], [Sofia.Stmt])
  m (Sofia.FunctionStmt _ p b) = (p, b)

findGlobalLets
  :: String
  -> Sofia.Program
  -> [Sofia.Expr]
findGlobalLets name = map m . filter flt where
  flt (Sofia.GlobalLetStmt n _) = n == name
  flt _ = False

  m (Sofia.GlobalLetStmt _ v) = v

printFunctionName :: Maybe String -> String
printFunctionName Nothing  = "<anonymous>"
printFunctionName (Just n) = '"':n ++ "\""

processFunctionDefinition
  :: Sofia.Program                                  -- sofia program
  -> Maybe String                                   -- optional function name
  -> [Fqs.Type]                                     -- list of parameters
  -> Fqs.Program                                    -- partial FQS program
  -> ([String], [(String, Integer)], [Sofia.Stmt])  -- (parameter names, function body)
  -> [Either ErrorTree (Fqs.Program, Integer)]      -- list of FQS programs with ids of functions
processFunctionDefinition _ name params _ (defParams, _, _) | length params /= length defParams = return
  $ Left $ ErrorTree ("definition of the function "
    ++ printFunctionName name
    ++ " has incorrect amount of parameters ("
    ++ show (length defParams)
    ++ " vs needed "
    ++ show (length params)
    ++ ")") []
processFunctionDefinition source name params result (defParams, defClosure, defBody) = do
  let params' = zip defParams params
  let (result', initialLocalScope) = foldl' processParam (result, []) params'
  let initialLocalScope' = initialLocalScope ++ defClosure

  stmts <- processSofiaStmts initialLocalScope' defBody result'

  return $ do
    (result'', body, ret) <- stmts
    return $ swap $ Fqs.addFunction (Fqs.Function name params' (fromMaybe (Fqs.UnionType $ singleton Fqs.UnitValue) ret) body) result''
  where
    processParam
      :: (Fqs.Program, [(String, Integer)])
      -> (String, Fqs.Type)
      -> (Fqs.Program, [(String, Integer)])
    processParam (r, ps) (n, t) = let
      (p', r') = Fqs.addLet (Fqs.Let n t False) r
      in (r', (n, p'):ps)

    processSofiaStmts
      :: [(String, Integer)]                                          -- local scope
      -> [Sofia.Stmt]                                                 -- sofia stmts
      -> Fqs.Program                                                  -- partial FQS program
      -> [Either ErrorTree (Fqs.Program, [Fqs.Stmt], Maybe Fqs.Type)] -- list of (FQS program, FQS stmts, return type)
    processSofiaStmts _ [] r = return $ return $ (r, [], Nothing)
    processSofiaStmts localScope (stmt:stmts) result = do
      branch <- processSofiaStmt localScope result stmt

      case branch of
        Right (result', stmt', Left newLocalScope) -> do
          next <- processSofiaStmts newLocalScope stmts result'

          return $ case next of
            Right (result'', stmts', t) -> Right (result'', stmt':stmts', t)
            err -> err

        Right (result', stmt', Right t) -> return $ Right (result', [stmt'], Just t)
        Left err -> return $ Left err

    processSofiaStmt
      :: [(String, Integer)]                                                             -- local scope
      -> Fqs.Program                                                                     -- partial FQS program
      -> Sofia.Stmt                                                                      -- sofia stmt
      -> [Either ErrorTree (Fqs.Program, Fqs.Stmt, Either [(String, Integer)] Fqs.Type)] -- list of (FQS program, FQS stmt, new local scope or return type)
    processSofiaStmt localScope result = processSofiaStmt' where
      processSofiaStmt' (Sofia.ExprStmt e) = map (fmap (\(r', ls', e', _) -> (r', Fqs.ExprStmt e', Left ls'))) $ processSofiaExpr' e
      processSofiaStmt' (Sofia.ReturnStmt e) = map (fmap (\(r', _, e', t) -> (r', Fqs.ReturnStmt e', Right t))) $ processSofiaExpr' e
      processSofiaStmt' (Sofia.LetStmt n e) = processSofiaLetStmt n e False
      processSofiaStmt' (Sofia.LetMutStmt n e) = processSofiaLetStmt n e True
      processSofiaStmt' (Sofia.IfStmt c t) = do
        c' <- processSofiaCondition c

        case c' of
          Right (r', ls', c', cond) -> if cond
            then do
              b <- processSofiaStmts ls' t result

              case b of
                Right (r', b', Nothing) -> return $ Right (r', Fqs.IfStmt c' b', Left ls')
                Right (r', b', Just t) -> return $ Right (r', Fqs.IfStmt c' b', Right t)
                Left err -> return $ Left err
            else do
              -- TODO idk what to do if b is an error branch
              -- b <- processSofiaStmts localScope t result
              --
              -- case b of
              --   Right (_, b', _) -> return $ Right (result, Fqs.IfStmt c' b', Left localScope)
              --   Left err -> return $ Left err
              return $ Right (r', ifFalseBranchStub, Left ls')
          Left err -> return $ Left err
      processSofiaStmt' (Sofia.IfElseStmt c t f) = do
        c' <- processSofiaCondition c

        case c' of
          Right (r', ls', c', cond) -> if cond
            then do
              b <- processSofiaStmts ls' t result

              case b of
                Right (r', b', Nothing) -> return $ Right (r', Fqs.IfElseStmt c' b' [ifFalseBranchStub], Left ls')
                Right (r', b', Just t) -> return $ Right (r', Fqs.IfElseStmt c' b' [ifFalseBranchStub], Right t)
                Left err -> return $ Left err
            else do
              b <- processSofiaStmts ls' f result

              case b of
                Right (r', b', Nothing) -> return $ Right (r', Fqs.IfElseStmt c' [ifFalseBranchStub] b', Left ls')
                Right (r', b', Just t) -> return $ Right (r', Fqs.IfElseStmt c' [ifFalseBranchStub] b', Right t)
                Left err -> return $ Left err
          Left err -> return $ Left err

      processSofiaExpr' = processSofiaExpr localScope result

      processSofiaLetStmt
        :: String
        -> Sofia.Expr
        -> Bool
        -> [Either ErrorTree (Fqs.Program, Fqs.Stmt, Either [(String, Integer)] Fqs.Type)]
      processSofiaLetStmt n e m = map (fmap f) $ processSofiaExpr' e where
        f (r, ls, e, t) = let
          (id, r') = Fqs.addLet (Fqs.Let n t m) r
          ls' = (n, id):ls
          in (r', Fqs.LetStmt id t e, Left ls')

      processSofiaCondition
        :: Sofia.Expr
        -> [Either ErrorTree (Fqs.Program, [(String, Integer)], Fqs.Expr, Bool)]
      processSofiaCondition c = do
        c' <- processSofiaExpr' c

        case c' of
          Right (r', ls', c', Fqs.UnionType ct) -> do
            ct' <- toList ct

            case ct' of
              Fqs.BoolValue cond -> return $ Right (r', ls', c', cond)
              _ -> return $ Left $ ErrorTree "not a Bool value passed as condition" []
          Left err -> return $ Left err

      -- TODO idk what to do if b is an error branch
      ifFalseBranchStub = Fqs.ExprStmt $ Fqs.ValueExpr $ Fqs.UnitValue

    processSofiaExpr
      :: [(String, Integer)]
      -> Fqs.Program
      -> Sofia.Expr
      -> [Either ErrorTree (Fqs.Program, [(String, Integer)], Fqs.Expr, Fqs.Type)]
    processSofiaExpr localScope result = processSofiaExpr' where
      processSofiaExpr' (Sofia.NameExpr "True") = return $ valueExpr $ Fqs.BoolValue True
      processSofiaExpr' (Sofia.NameExpr "False") = return $ valueExpr $ Fqs.BoolValue False
      processSofiaExpr' (Sofia.NameExpr n) = let
        localScopeNames = map (\id -> (id, Fqs.getLet id result)) $ map snd $ filter ((n ==) . fst) localScope
        localScopeShadowed = nubBy ((==) `on` (Fqs.getLetType . snd)) localScopeNames
        globalScope = processGlobalLets n result
        globalFunctions = convertGlobalFunctionToValue n

        fromLocalScope = map (\(id, Fqs.Let _ t _) -> Right (result, localScope, Fqs.NameExpr id, t)) localScopeShadowed
        fromGlobalScope = map fromGlobalScope' globalScope
        fromGlobalFunctions = map valueExpr globalFunctions

        fromGlobalScope'
          :: Either ErrorTree (Fqs.Program, Integer, Fqs.Type)
          -> Either ErrorTree (Fqs.Program, [(String, Integer)], Fqs.Expr, Fqs.Type)
        fromGlobalScope' (Right (result', id, t)) = Right (result', localScope, Fqs.NameExpr id, t)
        fromGlobalScope' (Left err) = Left err

        in case fromLocalScope ++ fromGlobalScope ++ fromGlobalFunctions of
          [] -> return $ Left $ ErrorTree ("there isn't name \"" ++ n ++ "\" in scope") []
          rs -> rs
      processSofiaExpr' (Sofia.CharLiteralExpr v) = return $ valueExpr $ Fqs.CharValue v
      processSofiaExpr' (Sofia.IntLiteralExpr v) = return $ valueExpr $ Fqs.IntegerValue v
      processSofiaExpr' (Sofia.DoubleLiteralExpr v) = return $ valueExpr $ Fqs.DoubleValue v
      processSofiaExpr'  Sofia.UndefinedExpr = return $ Left
        $ ErrorTree ("function " ++ printFunctionName name
          ++ " is undefined for parameters " ++ show params) []
      processSofiaExpr' (Sofia.SetExpr n e) = do
        (id, l) <- map (\id -> (id, Fqs.getLet id result)) $ map snd $ filter ((== n) . fst) localScope

        if Fqs.isLetMutable l
          then do
            e' <- processSofiaExpr' e

            case e' of
              Right (r', ls', e', t) -> return $ Right (Fqs.setLetType id t r', ls', Fqs.SetExpr id e', t)
              Left err -> return $ Left err
          else return $ Left $ ErrorTree ("name " ++ n ++ " is not mutable") []
--       processSofiaExpr' (Sofia.IfExpr c t f) = do
--         c' <- processSofiaExpr' c
--
--         case c' of
--           Right (r', ls', c', Fqs.UnionType ct) -> do
--             ct' <- toList ct
--
--             case ct' of
--               Fqs.BoolValue cond -> if cond
--                 then do
--                   b <- processSofiaStmts localScope t result
--
--                   -- require last stmt to be ExprStmt and result is its's value
--
--                   undefined
--                   -- case b of
--                   --   Right (r', b', Nothing) -> return $ Right (r', Fqs.IfStmt c' b', Left localScope)
--                   --   Right (r', b', Just t) -> return $ Right (r', Fqs.IfStmt c' b', Right t)
--                   --   Left err -> return $ Left err
--                 else undefined
--               _ -> return $ Left $ ErrorTree "not a Bool value passed as condition for if statement" []
--           Left err -> return $ Left err
      processSofiaExpr' (Sofia.CallExpr f ps) = do
        f' <- processSofiaExpr' f

        case f' of
          Right (r', ls', f', (Fqs.UnionType t')) -> do
            t' <- toList t'

            case t' of
              (Fqs.FunctionValue fn fp fc fb) -> do
                ps' <- processSofiaExprs ls' r' ps

                case ps' of
                  Right (r', ls', ps') -> do
                    f' <- processFunctionDefinition source fn (map snd ps') r' (fp, fc, fb)

                    case f' of
                      Right (r', id) -> return $ Right (r', ls', Fqs.CallExpr id $ map fst ps', Fqs.getFunctionType $ Fqs.getFunction id r')
                      Left err -> return $ Left err
                  Left err -> return $ Left err
              _ -> return $ Left $ ErrorTree "not a function called" []
          Left err -> return $ Left err
      processSofiaExpr' (Sofia.FunctionExpr p b) = return $ valueExpr $ Fqs.FunctionValue Nothing p localScope b

      valueExpr :: Fqs.Value -> Either ErrorTree (Fqs.Program, [(String, Integer)], Fqs.Expr, Fqs.Type)
      valueExpr v = Right (result, localScope, Fqs.ValueExpr v, Fqs.UnionType $ singleton v)

    processGlobalLets
      :: String
      -> Fqs.Program
      -> [Either ErrorTree (Fqs.Program, Integer, Fqs.Type)]
    processGlobalLets name result = do
      v <- findGlobalLets name source
      v' <- processSofiaExpr [] result v

      return $ case v' of
        Right (result', [], v', t') -> let
          (id, result'') = Fqs.addLet (Fqs.Let name t' False) result'
          in Right (Fqs.addGlobalLet (id, v') result'', id, t')
        Left err -> Left err

        Right a@(_, _, _, _) -> error $ "WTF non empty new local scope for global scope " ++ show a

    convertGlobalFunctionToValue :: String -> [Fqs.Value]
    convertGlobalFunctionToValue name = map m $ findFunctionDefinitions name source where
      m (ps, b) = Fqs.FunctionValue (Just name) ps [] b

    processSofiaExprs
      :: [(String, Integer)]
      -> Fqs.Program
      -> [Sofia.Expr]
      -> [Either ErrorTree (Fqs.Program, [(String, Integer)], [(Fqs.Expr, Fqs.Type)])]
    processSofiaExprs ls r = foldM f (Right (r, ls, [])) where
      f (Right (r, ls, es)) e = do
        res <- processSofiaExpr ls r e

        case res of
          Right (r', ls', e', t) -> return $ Right (r', ls', (e', t):es)
          Left err -> return $ Left err
