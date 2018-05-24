module Language.VCGens.LinGeneralization where

import Debug.Trace

import Language.Logic.Types
import Language.WhileSA.Types
import Language.VCGens.Base

lin :: VOp -> StmSA -> (LExpr, LExpr, LExpr, LExpr, LExpr)
lin _ SskipSA         =
  (BtrueSA, BtrueSA, BfalseSA, BfalseSA, BtrueSA)
lin _ (SassSA x e)    =
  (BeqSA (VariableSA x) e, BtrueSA, BfalseSA, BfalseSA, BtrueSA)
lin _ (SassumeSA e)   =
  (BtrueSA, e, BfalseSA, BfalseSA, BtrueSA)
lin op (SifSA b st sf) =
  (mkOr (mkAnd b phi1) (mkAnd (mkNeg b) phi2)
  ,mkOr (mkAnd b gamma1) (mkAnd (mkNeg b) gamma2)
  ,mkOr (mkAnd b omg1) (mkAnd (mkNeg b) omg2)
  ,mkOr (mkAnd b mu1) (mkAnd (mkNeg b) mu2)
  ,mkAnd (mkImpl b delta1) (mkImpl (mkNeg b) delta2))
  where
    (phi1,gamma1,omg1,mu1,delta1) = lin op st
    (phi2,gamma2,omg2,mu2,delta2) = lin op sf
lin op SthrowSA = (BfalseSA,BfalseSA,BtrueSA,BtrueSA,BtrueSA)
lin op (SforInvSA i b u inv s) =
  trace "VC Geneneration with FOR loops is still experimental"
  (BtrueSA,
   mkAnd inv (mkNeg b),
   omg1,
   mu1,
   mkBigAnd [applyRnm i inv,
    mkImpl (mkAnd inv b)
     (mkAnd delta1 (mkImpl (mkAnd phi1 gamma1) (applyRnm u inv)))]
   )
  where
    (phi1,gamma1,omg1,mu1,delta1) = lin op s
lin op (StrySA s1 s2) | op == VCP || op == VCPA = -- Partial Context
  (mkOr phi1 (mkAnd omg1 phi2)
  ,mkOr gamma1 (mkAnd mu1 gamma2)
  ,mkAnd omg1 omg2
  ,mkAnd mu1 mu2
  ,mkAnd delta1 (mkImpl (mkAnd omg1 mu1) delta2))
  where
    (phi1,gamma1,omg1,mu1,delta1) = lin op s1
    (phi2,gamma2,omg2,mu2,delta2) = lin op s2
lin op (StrySA s1 s2) | op == VCG || op == VCGA = -- Global Context
  (mkOr phi1 (mkAnd omg1 phi2)
  ,mkOr gamma1 (mkAnd mu1 gamma2)
  ,mkAnd omg1 omg2
  ,mkAnd mu1 mu2
  ,mkAnd delta1 (mkImpl mu1 delta2))
  where
    (phi1,gamma1,omg1,mu1,delta1) = lin op s1
    (phi2,gamma2,omg2,mu2,delta2) = lin op s2    
lin op (ScompSA s1 s2) | op == VCP || op == VCPA = -- Partial Context
  (mkAnd phi1 phi2
  , mkAnd gamma1 gamma2
  , mkOr omg1 (mkAnd phi1 omg2)
  , mkOr mu1 (mkAnd gamma1 mu2)
  , mkAnd delta1 (mkImpl (mkAnd phi1 gamma1) delta2))
  where
    (phi1,gamma1,omg1,mu1,delta1) = lin op s1
    (phi2,gamma2,omg2,mu2,delta2) = lin op s2
lin op (ScompSA s1 s2) | op == VCG || op == VCGA = -- Global Context
  (mkAnd phi1 phi2
  , mkAnd gamma1 gamma2
  , mkOr omg1 (mkAnd phi1 omg2)
  , mkOr mu1 (mkAnd gamma1 mu2)
  , mkAnd delta1 (mkImpl gamma1 delta2))
  where
    (phi1,gamma1,omg1,mu1,delta1) = lin op s1
    (phi2,gamma2,omg2,mu2,delta2) = lin op s2
lin op (SassertSA e) | op == VCP || op == VCG = -- Assert not in context
  (BtrueSA, BtrueSA, BfalseSA, BfalseSA, e) 
lin op (SassertSA e) | op == VCPA || op == VCGA = -- Assert in context
  (BtrueSA, e, BfalseSA, BfalseSA, e)
