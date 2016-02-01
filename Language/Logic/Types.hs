module Language.Logic.Types where

import Language.WhileSA.Types

type Expr = BexpSA

type SetExpr = [Expr]

mkAnd :: Expr -> Expr -> Expr
mkAnd BtrueSA b = b
mkAnd a BtrueSA = a
mkAnd BfalseSA b = BfalseSA
mkAnd a BfalseSA = BfalseSA
mkAnd a b = BandSA a b

mkImpl :: Expr -> Expr -> Expr
mkImpl a BtrueSA = BtrueSA
mkImpl BfalseSA b = BtrueSA
mkImpl BtrueSA b = b
mkImpl a b = BimplSA a b

mkOr :: Expr -> Expr -> Expr
mkOr BfalseSA b = b
mkOr a BfalseSA = a
mkOr BtrueSA b = BtrueSA
mkOr a BtrueSA = BtrueSA
mkOr a@(BnegSA x) b = if x == b then BtrueSA else BorSA a b
mkOr a b@(BnegSA x) = if a == x then BtrueSA else BorSA a b
mkOr a b = BorSA a b

mkBigAnd :: [Expr] -> Expr
mkBigAnd = foldr mkAnd BtrueSA

mkNeg :: Expr -> Expr
mkNeg (BnegSA e) = e
mkNeg e          = BnegSA e