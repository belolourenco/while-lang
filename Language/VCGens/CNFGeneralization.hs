module Language.VCGens.CNFGeneralization where

import Language.Logic.Types
import Language.WhileSA.Types
import Language.VCGens.Base

cnf :: VOp -> (LExpr, LExpr, LExpr,StmSA)
                           -> (LExpr, LExpr, LExpr, LExpr, [LExpr])
cnf _ (pi,phi,gamma, SskipSA) =
  (BtrueSA, BtrueSA, mkImpl pi BfalseSA, mkImpl pi BfalseSA, [])
cnf _ (pi,phi,gamma, SassSA x e) =
  (mkImpl pi (BeqSA (VariableSA x) e), BtrueSA
  , mkImpl pi BfalseSA,mkImpl pi BfalseSA, [])
cnf _ (pi,phi,gamma, SassumeSA e) =
  (BtrueSA, mkImpl pi e
  , mkImpl pi BfalseSA, mkImpl pi BfalseSA, [])
cnf op (pi,phi,gamma, ScompSA s1 s2) =
  (mkAnd phi1 phi2
  , mkAnd gamma1 gamma2
  , mkOr omg1 (mkAnd phi1   omg2)
  , mkOr mu1  (mkAnd gamma1 mu2)
  , v1++v2)
  where (phi1,gamma1,omg1,mu1,v1) =
          cnf op (pi,phi,gamma,s1)
        (phi2,gamma2,omg2,mu2,v2) =
          cnf op (pi,mkAnd phi phi1, mkAnd gamma gamma1, s2)
cnf op (pi,phi,gamma, SifSA b st sf) =
  (mkAnd phi1 phi2
  , mkAnd gamma1 gamma2
  , mkAnd omg1 omg2
  , mkAnd mu1 mu2
  , v1++v2)
  where (phi1,gamma1,omg1,mu1,v1) =
          cnf op (mkAnd pi b         , phi,gamma,st)
        (phi2,gamma2,omg2,mu2,v2) =
          cnf op (mkAnd pi (mkNeg b), phi,gamma,sf)
cnf op (pi,phi,gamma, StrySA s1 s2) =
  (mkOr phi1   (mkAnd omg1 phi2)
  ,mkOr gamma1 (mkAnd mu1 gamma2)
  ,mkAnd omg1 omg2
  ,mkAnd mu1 mu2
  ,v1++v2)
  where
    (phi1,gamma1,omg1,mu1,v1) =
      cnf op (pi,phi,gamma,s1)
    (phi2,gamma2,omg2,mu2,v2) =
      cnf op (pi,mkAnd phi omg1,mkAnd gamma mu1,s2)
cnf _ (pi,phi,psi, SthrowSA) =
  (mkImpl pi BfalseSA, mkImpl pi BfalseSA, BtrueSA, BtrueSA, [])
cnf VCP (pi,phi,gamma, SassertSA e) =
  (BtrueSA, BtrueSA
  , mkImpl pi BfalseSA , mkImpl pi BfalseSA
  , [mkImpl (mkAnd phi gamma) (mkImpl pi e)])
cnf VCPA (pi,phi,gamma, SassertSA e) =
  (BtrueSA, mkImpl pi e
  , mkImpl pi BfalseSA, mkImpl pi BfalseSA
  , [mkImpl (mkAnd phi gamma) (mkImpl pi e)])
cnf VCG (pi,phi,gamma, SassertSA e) =
  (BtrueSA, BtrueSA
  , mkImpl pi BfalseSA, mkImpl pi BfalseSA
  , [mkImpl gamma (mkImpl pi e)])
cnf VCGA  (pi,phi,gamma, SassertSA e) =
  (BtrueSA, mkImpl pi e
  , mkImpl pi BfalseSA, mkImpl pi BfalseSA
  , [mkImpl gamma (mkImpl pi e)])
