module Language.Logic.Why3Wrapper where

import Data.Text hiding (map)
import Language.Why3.AST
import Language.Why3.PP

p = pack

iVar :: String -> Expr
iVar s = App (p s) []

iVal :: Integer -> Expr
iVal i = Lit $ Integer i

bVal :: Bool -> Expr
bVal b = Lit $ Bool b

mkAdd :: Expr -> Expr -> Expr
mkAdd e1 e2 = App (p "+") [e1,e2]

mkSub :: Expr -> Expr -> Expr
mkSub e1 e2 = App (p "-") [e1,e2]

mkMul :: Expr -> Expr -> Expr
mkMul e1 e2 = App (p "*") [e1,e2]

mkDiv :: Expr -> Expr -> Expr
mkDiv e1 e2 = App (p "div") [e1,e2]

mkTrue :: Expr
mkTrue = bVal True

mkFalse :: Expr
mkFalse = bVal False

mkEq :: Expr -> Expr -> Expr
mkEq e1 e2 = App (p "=") [e1,e2]

mkLeq :: Expr -> Expr -> Expr
mkLeq e1 e2 = App (p "<=") [e1,e2]

mkL :: Expr -> Expr -> Expr
mkL e1 e2 = App (p "<") [e1,e2]

mkGeq :: Expr -> Expr -> Expr
mkGeq e1 e2 = App (p ">=") [e1,e2]

mkG :: Expr -> Expr -> Expr
mkG e1 e2 = App (p ">") [e1,e2]

mkNeg :: Expr -> Expr
mkNeg e = Not e

mkAnd :: Expr -> Expr -> Expr
mkAnd e1 e2 = Conn And e1 e2

mkOr :: Expr -> Expr -> Expr
mkOr e1 e2 = Conn Or e1 e2

mkImpl :: Expr -> Expr -> Expr
mkImpl e1 e2 = Conn Implies e1 e2

mkGoal :: String -> Expr -> Decl
mkGoal s e = Goal (p s) e

mkQuant :: [(Name,Type)] -> Expr -> Expr
mkQuant t e = Quant Forall t [] e

mkQuantIntVars :: [String] -> [(Name,Type)]
mkQuantIntVars = map (\s -> (p s, TyCon (p "int") []))

