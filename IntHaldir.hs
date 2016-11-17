{-# LANGUAGE FlexibleContexts #-}

module IntHaldir where

import Control.Monad
import Control.Monad.Error
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Control.Monad.State

import AbsHaldir

data Value =
    VInt Integer |
    VBool Bool |
    VList [Integer] |
    VFun [Ident] Env Exp  -- [args], env at definition time, body
    deriving Eq

instance Show Value where
    show (VInt i) = show i
    show (VBool b) = show b
    show (VList l) = show l
    show (VFun args env exp) = show args ++ " {" ++ show exp ++ "}"

type Env = M.Map Ident Value
type Eval a = ErrorT String (State Env) a

-- Built-ins
initialEnv :: [(Ident, Value)]
initialEnv = [
    (Ident "negate", VFun [(Ident "x")] M.empty (ENeg (EVar (Ident "x")))),
    (Ident "eq", VFun [(Ident "x"), (Ident "y")] M.empty (EEq (EVar (Ident "x")) (EVar (Ident "y")))),
    (Ident "neq", VFun [(Ident "x"), (Ident "y")] M.empty (ENeq (EVar (Ident "x")) (EVar (Ident "y")))),
    (Ident "add", VFun [(Ident "x"), (Ident "y")] M.empty (EAdd (EVar (Ident "x")) (EVar (Ident "y")))),
    (Ident "sub", VFun [(Ident "x"), (Ident "y")] M.empty (ESub (EVar (Ident "x")) (EVar (Ident "y")))),
    (Ident "mul", VFun [(Ident "x"), (Ident "y")] M.empty (EMul (EVar (Ident "x")) (EVar (Ident "y")))),

    (Ident "negateB", VFun [(Ident "x")] M.empty (ENegB (EVar (Ident "x")))),
    (Ident "eqB", VFun [(Ident "x"), (Ident "y")] M.empty (EEqB (EVar (Ident "x")) (EVar (Ident "y")))),
    (Ident "neqB", VFun [(Ident "x"), (Ident "y")] M.empty (ENeqB (EVar (Ident "x")) (EVar (Ident "y")))),
    (Ident "lt", VFun [(Ident "x"), (Ident "y")] M.empty (ELt (EVar (Ident "x")) (EVar (Ident "y")))),
    (Ident "lte", VFun [(Ident "x"), (Ident "y")] M.empty (ELte (EVar (Ident "x")) (EVar (Ident "y")))),
    (Ident "gt", VFun [(Ident "x"), (Ident "y")] M.empty (EGt (EVar (Ident "x")) (EVar (Ident "y")))),
    (Ident "gte", VFun [(Ident "x"), (Ident "y")] M.empty (EGte (EVar (Ident "x")) (EVar (Ident "y")))),

    (Ident "empty", VFun [(Ident "l")] M.empty (EEmpty (EVar (Ident "l")))),
    (Ident "head", VFun [(Ident "l")] M.empty (EHead (EVar (Ident "l")))),
    (Ident "tail", VFun [(Ident "l")] M.empty (ETail (EVar (Ident "l")))),
    (Ident "cons", VFun [(Ident "x"), (Ident "l")] M.empty (ECons (EVar (Ident "x")) (EVar (Ident "l"))))
    ]

runS s = evalState s (M.fromList initialEnv)
runEval = runS . runErrorT

interpret :: Program -> Either String [Value]
interpret (Prog l) =  runEval $ mapM evalInst l

evalInst:: Inst -> Eval Value
evalInst inst = catchError (eval inst) handler where
    handler e = throwError $ "Error in\n" ++ show inst ++ "\n" ++ e

eval :: Inst -> Eval Value
eval (DInst x) = evalDef x
eval (EInst x) = evalExp x

evalDef :: Def -> Eval Value

-- Variable definition
evalDef (Defin [(IdT name _)] e) = do
    env <- get
    value <- evalExp e
    put $ M.insert name value env
    return value

-- Function definition, `args` is not empty.
evalDef (Defin ((IdT name _):args) e) = do
    env <- get
    let argNames = getArgNames args
        newEnv = M.insert name (VFun argNames newEnv e) env
    put newEnv
    return $ VFun argNames newEnv e

evalExp :: Exp -> Eval Value

evalExp (EInt n) = return $ VInt n
evalExp (EBool BTrue) = return $ VBool True
evalExp (EBool BFalse) = return $ VBool False
evalExp (List l) = return $ VList l

-- Built-in arithmetic operations.
evalExp (ENeg xE) = do
    xV <- evalExp xE
    case xV of
        VInt x -> return $ VInt (negate x)

evalExp (EEq xE yE) = do
    xV <- evalExp xE
    yV <- evalExp yE
    return $ VBool (xV == yV)

evalExp (ENeq xE yE) = do
    xV <- evalExp xE
    yV <- evalExp yE
    return $ VBool (xV /= yV)

evalExp (EAdd xE yE) = do
    xV <- evalExp xE
    yV <- evalExp yE
    case (xV, yV) of
        (VInt x, VInt y) -> return $ VInt (x + y)

evalExp (ESub xE yE) = do
    xV <- evalExp xE
    yV <- evalExp yE
    case (xV, yV) of
        (VInt x, VInt y) -> return $ VInt (x - y)

evalExp (EMul xE yE) = do
    xV <- evalExp xE
    yV <- evalExp yE
    case (xV, yV) of
        (VInt x, VInt y) -> return $ VInt (x * y)

-- Built-in comparisions.
evalExp (ENegB xE) = do
    xV <- evalExp xE
    case xV of
        VBool x -> return $ VBool (not x)

evalExp (EEqB xE yE) = do
    xV <- evalExp xE
    yV <- evalExp yE
    return $ VBool (xV == yV)

evalExp (ENeqB xE yE) = do
    xV <- evalExp xE
    yV <- evalExp yE
    return $ VBool (xV /= yV)

evalExp (ELt xE yE) = do
    xV <- evalExp xE
    yV <- evalExp yE
    case (xV, yV) of
        (VInt x, VInt y) -> return $ VBool (x < y)

evalExp (ELte xE yE) = do
    xV <- evalExp xE
    yV <- evalExp yE
    case (xV, yV) of
        (VInt x, VInt y) -> return $ VBool (x <= y)

evalExp (EGt xE yE) = do
    xV <- evalExp xE
    yV <- evalExp yE
    case (xV, yV) of
        (VInt x, VInt y) -> return $ VBool (x > y)

evalExp (EGte xE yE) = do
    xV <- evalExp xE
    yV <- evalExp yE
    case (xV, yV) of
        (VInt x, VInt y) -> return $ VBool (x >= y)

-- Built-in list operations.
evalExp (EEmpty lE) = do
    lV <- evalExp lE
    case lV of
        VList l -> return $ VBool (null l)

evalExp (EHead lE) = do
    lV <- evalExp lE
    case lV of
        VList [] -> throwError $ "head: empty list"
        VList l -> return $ VInt (head l)

evalExp (ETail lE) = do
    lV <- evalExp lE
    case lV of
        VList [] -> throwError $ "tail: empty list"
        VList l -> return $ VList (tail l)

evalExp (ECons xE lE) = do
    xV <- evalExp xE
    lV <- evalExp lE
    case (lV, xV) of
        (VList l, VInt x) -> return $ VList (x:l)

evalExp (EVar name) = do
    env <- get
    return $ fromJust (M.lookup name env)

evalExp (EApp f e) = do
    fV <- evalExp f
    eV <- evalExp e
    env <- get
    case fV of
        VFun [arg] fEnv exp -> do
            let fEnvApplied = M.insert arg eV fEnv
            put fEnvApplied
            result <- evalExp exp
            put env
            return result
        VFun (arg:args) fEnv exp -> do
            let fEnvApplied = M.insert arg eV fEnv
            return $ VFun args fEnvApplied exp

evalExp exp@(ECond cond t f) = do
    cond <- evalExp cond
    case cond of
        VBool True -> evalExp t
        VBool False -> evalExp f

evalExp (ELet defs e) = do
    env <- get
    mapM evalDef defs
    eV <- evalExp e
    put env
    return eV

evalExp (EAnon _ argsT e) = do
    env <- get
    return $ VFun (getArgNames argsT) env e

getArgNames :: [IdentT] -> [Ident]
getArgNames args = foldr (\(IdT n _) acc -> (n:acc)) [] args