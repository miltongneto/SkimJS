import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool	
evalExpr env (StringLit string) = return $ String string
evalExpr env (NullLit) = return Nil

evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    case v1 of
        (Retorno va) -> do
            case v2 of
                (Retorno vb) -> infixOp env op va vb
                _ -> infixOp env op va v2
        _ -> do
            case v2 of
                (Retorno vb) -> infixOp env op v1 vb
                _ -> infixOp env op v1 v2

-- Fazer AssignExpr para os outros casos
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    find <- stateLookup env var
    e <- evalExpr env expr
    case find of
      ErroVar var -> inserirGlobalVar var e
      _ -> setVar var e

--feitas por nos

evalExpr env (ArrayLit []) = return (Lista [])
evalExpr env (ArrayLit l) = evalList env l (Lista [])

---evalExpr env (ArrayLit (x:xs)) = evalExpr env x >> evalExpr env (ArrayLit xs)

evalExpr env (CallExpr (DotRef exp (Id id)) params) =   do
                                                         lista <- evalExpr env exp
                                                         case lista of
                                                            Lista array -> do
                                                                            case id of 
                                                                                "head" -> myHead env lista
                                                                                "tail" -> return $ myTail env lista
                                                                                "concat" -> myConcat env array params
                                                                                "len" -> len env lista
                                                                              
                                                          


--evalExpr env (CallExpr ())

evalExpr env (CallExpr (VarRef (Id name)) params) = do 
                                                      f <- stateLookup env name
                                                      case f of
                                                          (Function id args sts) -> do
                                                              pushScope
                                                              verificarParams env args params
                                                              result <- evalStmt env (BlockStmt sts)
                                                              popScope
                                                              return result

evalExpr env (DotRef exp (Id id)) = do {
                                  c<-evalExpr env exp;
                                  case id of 
                                      "head" -> myHead env c
                                      "tail" -> return $ myTail env c
                                      "len" -> len env c
                                  }
evalExpr env (BracketRef expr1 expr2) = do
    evaluedExpr1 <- evalExpr env expr1
    evaluedExpr2 <- evalExpr env expr2
    getElement env evaluedExpr1 evaluedExpr2

getElement env (Lista []) (Int n) = return Nil
getElement env (Lista (l:ls)) (Int 0) = return l
getElement env (Lista (l:ls)) (Int n) = do
    getElement env (Lista ls) (Int (n-1))

evalList env [] (Lista l) = return (Lista l)
evalList env (x:xs) (Lista l) = do
                                  lis <- evalExpr env x
                                  evalList env xs (Lista (l++[lis]))

myHead env (Lista []) = return Nil
myHead env (Lista (x:xs)) = return x

myTail env (Lista (x:xs)) = (Lista xs)

len env (Lista []) = return (Int 0)
len env (Lista l) = return (Int (lenn l))

lenn l = foldr (soma1) 0 l
soma1 x y = 1 + y 

myConcat env l [] = return (Lista l)
myConcat env l1 (p:ps) = do
  result <- evalExpr env p 
  case result of
    (Lista l2) -> myConcat env (l1++l2) ps
    v -> myConcat env (l1 ++ [v]) ps

--myConcat env [] (Lista l2) = return (Lista l2)
--myConcat env (x:xs) (Lista l2) = do
  --                                  c <- evalExpr env x
    --                                (Lista l) <- myConcat env xs c
      --                              return $ [l2++l]

--somaListas :: Listas -> Listas -> Listas
--somaListas [] []         = []
--somaListas l1 []         = l1
--somaListas [] l2         = l2
--somaListas (x:xs) (y:ys) = [x+y] ++ somaListas xs ys

inserirLocalVar :: String -> Value -> StateTransformer Value                                                    
inserirLocalVar var v = ST $ \(h:t) -> (v, (insert var v h):t)

inserirGlobalVar :: String -> Value -> StateTransformer Value
inserirGlobalVar var v = ST $ \s -> (v, inserirGlobalAux var v s)

inserirGlobalAux :: String -> Value -> StateT -> StateT
inserirGlobalAux var val (s:scopos) = 
    if (scopos == []) 
        then (insert var val s):[] 
    else s:(inserirGlobalAux var val scopos)

verificarParams env [] [] = return Nil
verificarParams env ((Id arg):args) (p:params) = do
                                                   r <- evalExpr env p
                                                   inserirLocalVar arg r
                                                   verificarParams env args params
verificarParams env _ _ = return Erro

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)


evalStmt env (ExprStmt expr) = evalExpr env expr

evalStmt env (IfSingleStmt exp st) = do
                                       resultExp <- evalExpr env exp
                                       if((boolAux resultExp) == True) 
                                          then evalStmt env st
                                       else return Nil
evalStmt env (IfStmt exp st1 st2) = do
                                       resultExp <- evalExpr env exp
                                       if((boolAux resultExp) == True) then evalStmt env st1
                                       else evalStmt env st2
                                                            
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt (h:t)) = do
                                   result <- evalStmt env h
                                   case result of
                                       Stop -> return Stop
                                       Retorno v -> return (Retorno v)
                                       _ -> evalStmt env (BlockStmt t)

-- utilizava if else antes, mas modificado para funcionar com os casos do Retorno Value                                  
evalStmt env (WhileStmt exp st) = do
                                    resultExp <- evalExpr env exp
                                    if ((boolAux resultExp) == True) then 
                                       do
                                         r <- evalStmt env st
                                         case r of
                                          Stop -> return Nil
                                          Retorno x -> return x
                                          _ -> evalStmt env (WhileStmt exp st)                                     
                                    else return Nil


evalStmt env (DoWhileStmt st exp) = do
                                     s <- evalStmt env st
                                     if (s == Stop) then return Nil
                                     else if (s == Continue) then evalStmt env (WhileStmt exp st)
                                     else 
                                       do
                                         resultExp <- evalExpr env exp
                                         if ((boolAux resultExp) == True) then evalStmt env (DoWhileStmt st exp)  
                                         else return Nil 

-- Verificar casos que ocorrem o break e "corrigir"
evalStmt env (BreakStmt m) = return Stop

--nao precisa, mas vo deixar pq eu fiz
evalStmt env (ContinueStmt Nothing) = return Continue

evalStmt env (ReturnStmt (Just x)) = do
                                       result <- evalExpr env x
                                       return (Retorno result) 
evalStmt env (ReturnStmt Nothing) = return (Retorno Nil)

evalStmt env (SwitchStmt exp []) = return Nil
evalStmt env (SwitchStmt exp ((CaseClause exp2 lst):xs)) = do
                                        resultExp <- evalExpr env exp
                                        resultExp2 <- evalExpr env exp2
                                        if (resultExp == resultExp2) then do
                                                                            result <- evalStmt env (BlockStmt lst)
                                                                            case result of
                                                                               Stop -> return (Retorno Nil)
                                                                               _ -> return result
                                        else evalStmt env (SwitchStmt exp xs)

evalStmt env (ThrowStmt exp) = evalExpr env exp

evalStmt env (FunctionStmt (Id name) args sts) = inserirGlobalVar name (Function (Id name) args sts)
                                                  -- do
                                                   --  let f = Function (Id name) args sts in ST $ (\s -> (f, insert name f s))

                                                  
--somaListas :: Listas -> Listas -> Listas
--somaListas [] []         = []
--somaListas l1 []         = l1
--somaListas [] l2         = l2
--somaListas (x:xs) (y:ys) = [x+y] ++ somaListas xs ys

pushScope :: StateTransformer Value
pushScope = ST $ \s -> (Nil, (Map.empty):s)

popScope :: StateTransformer Value
popScope = ST $ \s -> (Nil, (tail s))

boolAux (Bool b) = b
boolAux (Int i) | i == 0 = False
                | otherwise = True
boolAux (Var x) | x == "" = False
                | otherwise = True;
boolAux (String s) | s == "" = False
                   | otherwise = True            


-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

{- infixOp env op (Var id1) (Var id2) = do
                         var1 <- stateLookup env id1
                         var2 <- stateLookup env id2
                         case var1 of
                              ErroVar s -> 
-}
-- Testar e ajustar
infixOp env op (Var id) v2 = do
                           var <- stateLookup env id
                           case var of
                                ErroVar x -> ST $ \s -> (Int 12, s) 
                                val -> ST $ \s -> (Int 10, s) --infixOp env op val v2
infixOp env op v1 (Var id) = do
                           var <- stateLookup env id
                           case var of
                                ErroVar x -> ST $ \s -> (ErroVar id, s)
                                val -> infixOp env op v1 val

--
-- Environment and auxiliary functions
--

environment :: StateT
environment = [Map.empty]

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
     case procurarScopo s var of
        Nothing -> (ErroVar var, s)
        Just v -> (v, s)

procurarScopo :: StateT -> String -> Maybe Value
procurarScopo [] _ = Nothing
procurarScopo (s:scopos) var =
    case Map.lookup var s of
        Nothing -> procurarScopo scopos var
        Just val -> Just val

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> inserirLocalVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            inserirLocalVar id val


setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, (procurarNoScopo var val s))

procurarNoScopo var v [] = []
procurarNoScopo var v (s:stts) = case (Map.lookup var s) of
                                 Nothing -> s:(procurarNoScopo var v stts)
                                 Just x -> (insert var v s):stts

type StateT = [Map String Value]
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (v,[]) = ""
showResult (val, (def:defs)) =
     show val ++ "\n" ++ show (toList $ union def (Map.empty)) ++  "\n" ++ showResult (val, defs)

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f [Map.empty]

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements

