module Language.VCGens.LinGeneralization where

import Language.Logic.Types
import Language.WhileSA.Types
import Language.VCGens.Base

lin :: Context -> AsrtCtx -> StmSA -> (LExpr, LExpr, LExpr, LExpr, LExpr)
lin ctx asrt SskipSA         =
  (BtrueSA, BtrueSA, BfalseSA, BfalseSA, BtrueSA)
lin ctx asrt (SassSA x e)    =
  (BeqSA (VariableSA x) e, BtrueSA, BfalseSA, BfalseSA, BtrueSA)
lin ctx asrt (SassumeSA e)   =
  (BtrueSA, e, BfalseSA, BfalseSA, BtrueSA)
lin ctx asrt (SifSA b st sf) =
  (mkOr (mkAnd b phi1) (mkAnd (mkNeg b) phi2)
  ,mkOr (mkAnd b gamma1) (mkAnd (mkNeg b) gamma2)
  ,mkOr (mkAnd b omg1) (mkAnd (mkNeg b) omg2)
  ,mkOr (mkAnd b mu1) (mkAnd (mkNeg b) mu2)
  ,mkAnd (mkImpl b delta1) (mkImpl (mkNeg b) delta2))
  where
    (phi1,gamma1,omg1,mu1,delta1) = lin ctx asrt st
    (phi2,gamma2,omg2,mu2,delta2) = lin ctx asrt sf
lin ctx asrt SthrowSA = (BfalseSA,BfalseSA,BtrueSA,BtrueSA,BtrueSA)
lin Part asrt (StrySA s1 s2) = -- Partial Context
  (mkOr phi1 (mkAnd omg1 phi2)
  ,mkOr gamma1 (mkAnd mu1 gamma2)
  ,mkAnd omg1 omg2
  ,mkAnd mu1 mu2
  ,mkAnd delta1 (mkImpl (mkAnd omg1 mu1) delta2))
  where
    (phi1,gamma1,omg1,mu1,delta1) = lin Part asrt s1
    (phi2,gamma2,omg2,mu2,delta2) = lin Part asrt s2
lin Glob asrt (StrySA s1 s2) = -- Global Context
  (mkOr phi1 (mkAnd omg1 phi2)
  ,mkOr gamma1 (mkAnd mu1 gamma2)
  ,mkAnd omg1 omg2
  ,mkAnd mu1 mu2
  ,mkAnd delta1 (mkImpl mu1 delta2))
  where
    (phi1,gamma1,omg1,mu1,delta1) = lin Part asrt s1
    (phi2,gamma2,omg2,mu2,delta2) = lin Part asrt s2    
lin Part asrt (ScompSA s1 s2) = -- Partial Context
  (mkAnd phi1 phi2
  , mkAnd gamma1 gamma2
  , mkOr omg1 (mkAnd phi1 omg2)
  , mkOr mu1 (mkAnd gamma1 mu2)
  , mkAnd delta1 (mkImpl (mkAnd phi1 gamma1) delta2))
  where
    (phi1,gamma1,omg1,mu1,delta1) = lin Part asrt s1
    (phi2,gamma2,omg2,mu2,delta2) = lin Part asrt s2
lin Glob asrt (ScompSA s1 s2) = -- Global Context
  (mkAnd phi1 phi2
  , mkAnd gamma1 gamma2
  , mkOr omg1 (mkAnd phi1 omg2)
  , mkOr mu1 (mkAnd gamma1 mu2)
  , mkAnd delta1 (mkImpl gamma1 delta2))
  where
    (phi1,gamma1,omg1,mu1,delta1) = lin Glob asrt s1
    (phi2,gamma2,omg2,mu2,delta2) = lin Glob asrt s2
lin ctx AsrtNot (SassertSA e) = -- Assert not in context
  (BtrueSA, BtrueSA, BfalseSA, BfalseSA, e) 
lin ctx AsrtIn  (SassertSA e) = -- Assert in context
  (BtrueSA, e, BfalseSA, BfalseSA, e)
