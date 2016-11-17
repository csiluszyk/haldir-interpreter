{-# LANGUAGE FlexibleContexts #-}

module TErrorHaldir where

import Control.Monad
import Control.Monad.Error
import qualified Data.Map as M
import Control.Monad.State

import AbsHaldir

type Env = M.Map Ident Type
type TCM a = ErrorT String (State Env) a -- TypeControlMonad

initialEnv :: [(Ident, Type)]
initialEnv = [(Ident "negate", TFun TInt TInt),
              (Ident "eq", TFun TInt (TFun TInt TBool)),
              (Ident "neq", TFun TInt (TFun TInt TBool)),
              (Ident "add", TFun TInt (TFun TInt TInt)),
              (Ident "sub", TFun TInt (TFun TInt TInt)),
              (Ident "mul", TFun TInt (TFun TInt TInt)),

              (Ident "negateB", TFun TBool TBool),
              (Ident "eqB", TFun TBool (TFun TBool TBool)),
              (Ident "neqB", TFun TBool (TFun TBool TBool)),
              (Ident "lt", TFun TInt (TFun TInt TBool)),
              (Ident "lte", TFun TInt (TFun TInt TBool)),
              (Ident "gt", TFun TInt (TFun TInt TBool)),
              (Ident "gte", TFun TInt (TFun TInt TBool)),

              (Ident "empty", TFun TList TBool),
              (Ident "head", TFun TList TInt),
              (Ident "tail", TFun TList TList),
              (Ident "cons", TFun TInt (TFun TList TList))]

runS s = evalState s (M.fromList initialEnv)
runTCM = runS . runErrorT

checkTypeErrors :: Program -> Maybe String
checkTypeErrors (Prog l) =  case runTCM $ mapM checkTypeError l of
    Left err -> return err
    Right _ -> fail "OK (no errors)"

checkTypeError :: Inst -> TCM Type
checkTypeError inst = catchError (typeOf inst) handler where
    handler e = throwError $ "Type error in\n" ++ show inst ++ "\n" ++ e

typeOf :: Inst -> TCM Type
typeOf (DInst x) = typeOfDef x
typeOf (EInst x) = typeOfExp x

typeOfDef :: Def -> TCM Type

-- Variable defintion
typeOfDef def@(Defin [(IdT name t)] e) = do
    env <- get
    eT <- typeOfExp e
    if eT == t then do
        put $ M.insert name t env
        return t
    else
        throwError $ "In the definition " ++ show def ++
            "\nCouldn't match expected type '" ++ show t ++
            "' with actual type '" ++ show eT ++ "'\n"

-- Function definition, `args` is not empty.
typeOfDef (Defin ((IdT name fT):args) e) = do
    -- Checks if types of corresponding parameters are matching.
    let checkArgs _ [] = True
        checkArgs (TFun t1 t) ((IdT _ t2):argsR) =
            (t1 == t2) && (checkArgs t argsR)
        checkArgs _ _ = False

        expectedType (TFun _ t) [_] = t
        expectedType (TFun _ t) (_:argsR) = expectedType t argsR

    env <- get
    if (null $ show name) || (checkArgs fT args) then do
        let envWithArgs = foldl (\m (IdT n t) -> M.insert n t m) env args
            expectedT = expectedType fT args
        if not $ null $ show name then do
            -- Make local parameters and defined function available in `e`.
            put $ M.insert name fT envWithArgs
        else
            put envWithArgs
        computedType <- typeOfExp e
        if computedType == expectedT then do
            if not $ null $ show name then do
                put $ M.insert name fT env
            else
                return ()
            return fT
        else
            throwError $ "Couldn't match expected type '" ++
                show expectedT ++ "' with inferred type '" ++
                show computedType ++ "'\n"
    else
        throwError $ "Defined types for " ++ show name ++ " doesn't match"

typeOfExp :: Exp -> TCM Type

typeOfExp (EInt _) = return TInt
typeOfExp (EBool _) = return TBool
typeOfExp (List _) = return $ TList

typeOfExp (EVar name) = do
    env <- get
    case M.lookup name env of
        Just t -> return t
        Nothing -> throwError $ "Unknown variable " ++ show name ++ "\n"

typeOfExp exp@(EApp f e) = do
    fT <- typeOfExp f
    eT <- typeOfExp e
    case fT of
        (TFun firstT resultT)
            | eT == firstT -> return resultT
            | otherwise -> throwError $ "In the expression " ++ show exp ++
                "\nCouldn't match expected type '" ++ show firstT ++
                "' with actual type '" ++ show eT ++ "'\n"
        _ -> throwError $ "For expression\n" ++ show exp ++
                 "\nThe type of " ++ show f ++ ": " ++
                 show fT ++ " is not a function type"

typeOfExp exp@(ECond cond t f) = do
    condT <- typeOfExp cond
    tT <- typeOfExp t
    fT <- typeOfExp f
    case condT of
        TBool | tT == fT -> return tT
              | otherwise -> throwError $ "In the expression " ++ show exp ++
                    "\nCouldn't match expected type '" ++ show tT ++
                    "' with actual type '" ++ show fT ++ "'\n"
        _ -> throwError $ "if condition must be of type 'Bool'\n"

typeOfExp (ELet defs e) = do
    env <- get
    mapM typeOfDef defs
    eT <- typeOfExp e
    put env
    return eT

typeOfExp (EAnon t args e) = do
    typeOfDef (Defin ((IdT (Ident "") t):args) e)
