module Language.Logic.Types where

import Language.WhileSA.Types

type LExpr = BexpSA

mkAnd :: LExpr -> LExpr -> LExpr
mkAnd BtrueSA b = b
mkAnd a BtrueSA = a
mkAnd BfalseSA b = BfalseSA
mkAnd a BfalseSA = BfalseSA
mkAnd x@(BnegSA a) y@(BnegSA b)
  = if a == b then x else BandSA x y
mkAnd a b = BandSA a b

mkOr :: LExpr -> LExpr -> LExpr
mkOr BfalseSA b = b
mkOr a BfalseSA = a
mkOr BtrueSA b = BtrueSA
mkOr a BtrueSA = BtrueSA
mkOr a@(BnegSA x) b = if x == b then BtrueSA else BorSA a b
mkOr a b@(BnegSA x) = if a == x then BtrueSA else BorSA a b
mkOr a b = BorSA a b

mkBigAnd :: [LExpr] -> LExpr
mkBigAnd = foldr mkAnd BtrueSA

mkNeg :: LExpr -> LExpr
mkNeg (BnegSA e) = e
mkNeg e          = BnegSA e

mkImpl :: LExpr -> LExpr -> LExpr
mkImpl a BtrueSA = BtrueSA
mkImpl BfalseSA b = BtrueSA
mkImpl BtrueSA b = b
mkImpl a BfalseSA = mkNeg a
mkImpl a b = BimplSA a b
