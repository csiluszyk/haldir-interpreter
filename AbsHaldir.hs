module AbsHaldir where

newtype Ident = Ident String deriving (Eq, Ord, Read)

instance Show Ident where
    show (Ident x) = "'" ++ x ++ "'"

data Program = Prog [Inst]
    deriving (Eq, Ord, Show, Read)

data Inst = EInst Exp | DInst Def
    deriving (Eq, Ord, Show, Read)

data Type = TInt | TBool | TList | TFun Type Type
    deriving (Eq, Ord, Read)

instance Show Type where
    showsPrec d TInt = showString "Integer"
    showsPrec d TBool = showString "Bool"
    showsPrec d TList = showString "[Integer]"
    showsPrec d (TFun u v) = showParen (d > arr_prec) $
               showsPrec (arr_prec+1) u .
               showString " -> "       .
               showsPrec arr_prec v
            where arr_prec = 5

data Def = Defin [IdentT] Exp
    deriving (Eq, Ord, Show, Read)

data IdentT = IdT Ident Type
  deriving (Eq, Ord, Show, Read)

data Exp =
      EVar Ident
    | ECond Exp Exp Exp
    | ELet [Def] Exp
    | EAnon Type [IdentT] Exp
    | EApp Exp Exp
    | List [Integer]
    | EInt Integer
    | EBool Boolean
    -- Internals for built-in arithmetic operations.
    | ENeg Exp
    | EEq Exp Exp
    | ENeq Exp Exp
    | EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    -- Internals for built-in comparisions.
    | ENegB Exp
    | EEqB Exp Exp
    | ENeqB Exp Exp
    | ELt Exp Exp
    | ELte Exp Exp
    | EGt Exp Exp
    | EGte Exp Exp
    -- Internals for built-in list operations.
    | EEmpty Exp
    | EHead Exp
    | ETail Exp
    | ECons Exp Exp
  deriving (Eq, Ord, Show, Read)

data Boolean = BTrue | BFalse
  deriving (Eq, Ord, Show, Read)

