module Language.While.Utils.Utils where

import Data.List
import Language.While.Types
import Language.WhileSA.Types

-- | Returns all variables from an Aexp. The result may contain duplicates.
varsExp :: Aexp -> [Varname]
varsExp (Numeral i)   = []
varsExp (Variable n)  = [n]
varsExp (Aadd e1 e2)  = (varsExp e1) ++ (varsExp e2)
varsExp (Asub e1 e2)  = (varsExp e1) ++ (varsExp e2)
varsExp (Amul e1 e2)  = (varsExp e1) ++ (varsExp e2)
varsExp (Adiv e1 e2)  = (varsExp e1) ++ (varsExp e2)

-- | Returns all variables from an Bexp. The result may contain duplicates.
varsBexp :: Bexp -> [Varname]
varsBexp Btrue        = []
varsBexp Bfalse       = []
varsBexp (Beq e1 e2)  = (varsExp e1) ++ (varsExp e2)
varsBexp (Bleq e1 e2) = (varsExp e1) ++ (varsExp e2)
varsBexp (Bl e1 e2)   = (varsExp e1) ++ (varsExp e2)
varsBexp (Bg e1 e2)   = (varsExp e1) ++ (varsExp e2)
varsBexp (Bgeq e1 e2) = (varsExp e1) ++ (varsExp e2)
varsBexp (Bneg b)     = varsBexp b
varsBexp (Band b1 b2) = (varsBexp b1) ++ (varsBexp b2)

-- | Returns all variables from a Stm. The result does not contains duplicates.
vars :: Stm -> [Varname]
vars = nub.aux
  where
    aux :: Stm -> [Varname]
    aux (Sass v e)         = v:(varsExp e)
    aux Sskip              = []
    aux (Sassume b)        = varsBexp b
    aux (Sassert b)        = varsBexp b
    aux (Scomp s1 s2)      = (aux s1) ++ (aux s2)
    aux (Sif b s1 s2)      = (varsBexp b) ++ (aux s1) ++ (aux s2)
    aux (Swhile b s)       = (varsBexp b) ++ (aux s)
    aux (SwhileInv b i s)  = (varsBexp b) ++ (varsBexp i) ++ (aux s)
    aux (Stry s1 s2)       = (aux s1) ++ (aux s2)

-- | Returns all variables assigned in the given Stm
asgn :: Stm -> [Varname]
asgn = nub.aux
  where
    aux :: Stm -> [Varname]
    aux (Sass v e)        = [v]
    aux Sskip             = []
    aux (Sassume b)       = []
    aux (Sassert b)       = []
    aux (Scomp s1 s2)     = (aux s1) ++ (aux s2)
    aux (Sif b s1 s2)     = (aux s1) ++ (aux s2)
    aux (Swhile b s)      = (aux s)
    aux (SwhileInv b i s) = (aux s)
    aux (Stry s1 s2)      = (aux s1) ++ (aux s2)

rnmToAssign :: Rnm -> StmSA
rnmToAssign [] = SskipSA
rnmToAssign [(l,r)] = SassSA l (VariableSA r)
rnmToAssign ((l,r):ls) = ScompSA (SassSA l (VariableSA r))
                                 (rnmToAssign ls)