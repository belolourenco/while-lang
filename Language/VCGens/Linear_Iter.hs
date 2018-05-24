module Language.VCGens.Linear_Iter where

import Language.Logic.Types
import Language.WhileSA.Types
import Data.Maybe

-- This is just a temporary solution. See omnigraffle diagram for a cleaner solution

lin_iter :: StmSA -> (LExpr, LExpr, [LExpr])
lin_iter SskipSA                =
  (BtrueSA, BtrueSA, [])
lin_iter (SassSA x e)           =
  (BeqSA (VariableSA x) e, BtrueSA, [])
lin_iter (ScompSA s1 s2)        =
  (mkAnd psi1 psi2
  , mkAnd gamma1 (mkImpl psi1 gamma2)
  , t1 ++ t2)
  where (psi1,gamma1,t1) = lin_iter s1
        (psi2,gamma2,t2) = lin_iter s2
lin_iter (SifSA b st sf)        =
  (mkOr (mkAnd b psit) (mkAnd (mkNeg b) psif)
  , mkAnd (mkImpl b gammat) (mkImpl (mkNeg b) gammaf)
  , tt ++ tf)
  where (psit,gammat,tt) = lin_iter st
        (psif,gammaf,tf) = lin_iter sf
lin_iter (SforInvSA i b u inv s) =
  (mkAnd inv (mkNeg b)
  , applyRnm i inv
  , [mkImpl (mkAnd inv b) 
      (mkAnd gamma 
        (mkImpl psi (applyRnm u inv)))] ++ t)
  where (psi,gamma,t)    = lin_iter s
