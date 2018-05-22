module Language.VCGens.SPGeneralization where

import Language.Logic.Types
import Language.WhileSA.Types
import Language.VCGens.Base

-- SP Partial Context
sp :: VOp -> (LExpr, LExpr, StmSA)
                         -> (LExpr, LExpr, LExpr, LExpr, [LExpr])
sp _ (phi,psi, SskipSA) = 
        (BtrueSA, BtrueSA, BfalseSA, BfalseSA, [])
sp _ (phi,psi, SassSA x e) = 
        (BeqSA (VariableSA x) e, BtrueSA, BfalseSA, BfalseSA, [])
sp _ (phi,psi, SassumeSA e) = 
        (BtrueSA, e, BfalseSA, BfalseSA, [])
sp op (phi,psi, ScompSA s1 s2) = 
        (mkAnd f1 f2
        , mkAnd psi1 psi2
        , mkOr omega1 (mkAnd f1 omega2)
        , mkOr mu1 (mkAnd psi1 mu2)
        , v1++v2)
  where
    (f1,psi1,omega1,mu1,v1) = sp op (phi,psi,s1)
    (f2,psi2,omega2,mu2,v2) = sp op (mkAnd phi f1, mkAnd psi psi1, s2)
sp op (phi,psi, SifSA b st sf) = 
        (mkOr (mkAnd b ft) (mkAnd (BnegSA b) ff)
        , mkOr (mkAnd b psit) (mkAnd (BnegSA b) psif)
        , mkOr (mkAnd b omegat) (mkAnd (BnegSA b) omegaf)
        , mkOr (mkAnd b mut)    (mkAnd (BnegSA b) muf)
        , vt++vf)
  where
    (ft,psit,omegat,mut,vt) = sp op (mkAnd phi b, mkAnd psi b, st)
    (ff,psif,omegaf,muf,vf) = sp op (mkAnd phi (BnegSA b), mkAnd psi (BnegSA b), sf)
sp op (phi,psi, StrySA s1 s2) = 
        (mkOr f1 (mkAnd omega1 f2)
        , mkOr psi1 (mkAnd mu1 psi2)
        , mkAnd omega1 omega2
        , mkAnd mu1 mu2
        , v1++v2)
  where
    (f1,psi1,omega1,mu1,v1) = sp op (phi,psi,s1)
    (f2,psi2,omega2,mu2,v2) = sp op (mkAnd phi omega1,mkAnd psi mu1, s2)
sp _ (phi,psi, SthrowSA) = (BfalseSA, BfalseSA, BtrueSA, BtrueSA, [])
sp VCP (phi,psi, SassertSA e) = 
        (BtrueSA, BtrueSA, BfalseSA, BfalseSA, [mkImpl (mkAnd phi psi) e])
sp VCPA (phi,psi, SassertSA e) = 
        (BtrueSA, e, BfalseSA, BfalseSA, [mkImpl (mkAnd phi psi) e])
sp VCG (phi,psi, SassertSA e) = 
        (BtrueSA, BtrueSA, BfalseSA, BfalseSA, [mkImpl psi e])
sp VCGA (phi,psi, SassertSA e) = 
        (BtrueSA, e, BfalseSA, BfalseSA, [mkImpl psi e])
