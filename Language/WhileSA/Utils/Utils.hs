module Language.WhileSA.Utils.Utils where

import Data.List
import Language.While.Types
import Language.WhileSA.Types

-- | Returns all variables from an Aexp. The result may contain duplicates.
varsExp :: AexpSA -> [VarnameSA]
varsExp (NumeralSA i)   = []
varsExp (VariableSA n)  = [n]
varsExp (AaddSA e1 e2)  = (varsExp e1) ++ (varsExp e2)
varsExp (AsubSA e1 e2)  = (varsExp e1) ++ (varsExp e2)
varsExp (AmulSA e1 e2)  = (varsExp e1) ++ (varsExp e2)
varsExp (AdivSA e1 e2)  = (varsExp e1) ++ (varsExp e2)

-- | Returns all variables from an Bexp. The result may contain duplicates.
varsBexp :: BexpSA -> [VarnameSA]
varsBexp = nub.aux
  where
    aux BtrueSA        = []
    aux BfalseSA       = []
    aux (BVariableSA n) = [n]
    aux (BeqSA e1 e2)  = (varsExp e1) ++ (varsExp e2)
    aux (BleqSA e1 e2) = (varsExp e1) ++ (varsExp e2)
    aux (BlSA e1 e2)   = (varsExp e1) ++ (varsExp e2)
    aux (BgSA e1 e2)   = (varsExp e1) ++ (varsExp e2)
    aux (BgeqSA e1 e2) = (varsExp e1) ++ (varsExp e2)
    aux (BnegSA b)     = aux b
    aux (BandSA b1 b2) = (aux b1) ++ (aux b2)
    aux (BorSA b1 b2) = (aux b1) ++ (aux b2)
    aux (BimplSA b1 b2) = (aux b1) ++ (aux b2)

-- | Returns all variables from a Stm. The result does not contains duplicates.
vars :: StmSA -> [VarnameSA]
vars = nub.aux
  where
    aux :: StmSA -> [VarnameSA]
    aux (SassSA v e)         = v:(varsExp e)
    aux SskipSA              = []
    aux (SassumeSA b)        = varsBexp b
    aux (SassertSA b)        = varsBexp b
    aux (ScompSA s1 s2)      = (aux s1) ++ (aux s2)
    aux (SifSA b s1 s2)      = (varsBexp b) ++ (aux s1) ++ (aux s2)
    aux (SforSA i b u s)     = (varsBexp b) ++ (aux s)
    aux (SforInvSA i b u inv s) = (varsBexp b) ++ (varsBexp inv) ++ (aux s)
    aux (StrySA s1 s2)       = (aux s1) ++ (aux s2)

-- | Returns all variables assigned in the given Stm
asgn :: StmSA -> [VarnameSA]
asgn = nub.aux
  where
    aux :: StmSA -> [VarnameSA]
    aux (SassSA v e)        = [v]
    aux SskipSA             = []
    aux (SassumeSA b)       = []
    aux (SassertSA b)       = []
    aux (ScompSA s1 s2)     = (aux s1) ++ (aux s2)
    aux (SifSA b s1 s2)     = (aux s1) ++ (aux s2)
    aux (SforSA i b u s)      = (aux s)
    aux (SforInvSA i b u inv s) = (aux s)
    aux (StrySA s1 s2)      = (aux s1) ++ (aux s2)