module Language.Logic.Why3Encoder where

import Text.PrettyPrint

import Language.Why3.AST

import Language.Logic.Why3Wrapper

import Language.WhileSA.Types
import Language.WhileSA.Utils.Utils
import Language.WhileSA.PrettyPrinter
import Language.While.PrettyPrinter
import Language.Logic.Types 
  hiding( mkAnd
        , mkImpl
        , mkOr
        , mkBigAnd
        , mkNeg
        , Expr)

useImportInt :: Decl
useImportInt = Use (Just Import) (p "int.Int") Nothing

useImportDiv :: Decl
useImportDiv = Use (Just Import) (p "int.ComputerDivision") Nothing

setExpr2why3theory :: SetExpr -> Theory
setExpr2why3theory s = Theory (p "WhileLangVCs") 
                              (useImportInt
                                :useImportDiv
                                :(setExpr2why3decl s))

setExpr2why3decl :: SetExpr -> [Decl]
setExpr2why3decl = (aux 0).setExpr2why3expr
  where aux :: Int -> [Expr] -> [Decl]
        aux _ []    = []
        aux i (h:t) = (mkGoal ("G" ++ (show i)) h)
                        :(aux (i+1) t)

saV2str :: VarnameSA -> String
saV2str = render.pretty

setExpr2why3expr :: SetExpr -> [Expr]
setExpr2why3expr s = map aux s
  where aux e = mkQuant (mkQuantIntVars $ map saV2str (varsBexp e)) 
                        (bExpr2why3 e)

aExpr2why3 :: AexpSA -> Expr
aExpr2why3 (NumeralSA i)  = iVal i
aExpr2why3 (VariableSA v) = iVar.saV2str $ v
aExpr2why3 (AaddSA e1 e2) = mkAdd (aExpr2why3 e1) (aExpr2why3 e2)
aExpr2why3 (AsubSA e1 e2) = mkSub (aExpr2why3 e1) (aExpr2why3 e2)
aExpr2why3 (AmulSA e1 e2) = mkMul (aExpr2why3 e1) (aExpr2why3 e2)
aExpr2why3 (AdivSA e1 e2) = mkDiv (aExpr2why3 e1) (aExpr2why3 e2)

bExpr2why3 :: BexpSA -> Expr
bExpr2why3 BtrueSA         = mkTrue
bExpr2why3 BfalseSA        = mkFalse
bExpr2why3 (BeqSA e1 e2)   = mkEq (aExpr2why3 e1) (aExpr2why3 e2)
bExpr2why3 (BleqSA e1 e2)  = mkLeq (aExpr2why3 e1) (aExpr2why3 e2)
bExpr2why3 (BlSA e1 e2)    = mkL (aExpr2why3 e1) (aExpr2why3 e2)
bExpr2why3 (BgSA e1 e2)    = mkG (aExpr2why3 e1) (aExpr2why3 e2)
bExpr2why3 (BgeqSA e1 e2)  = mkGeq (aExpr2why3 e1) (aExpr2why3 e2)
bExpr2why3 (BnegSA b)      = mkNeg (bExpr2why3 b)
bExpr2why3 (BandSA b1 b2)  = mkAnd (bExpr2why3 b1) (bExpr2why3 b2)
bExpr2why3 (BorSA b1 b2)   = mkOr (bExpr2why3 b1) (bExpr2why3 b2)
bExpr2why3 (BimplSA b1 b2) = mkImpl (bExpr2why3 b1) (bExpr2why3 b2)