module Language.VCGens.SPGeneralization where

import Language.Logic.Types
import Language.WhileSA.Types

psp :: (Expr, Expr, StmSA) -> (Expr, Expr, SetExpr)
psp (phi,psi, SskipSA) = (BtrueSA, BtrueSA, [])
psp (phi,psi, SassSA x e) = (BeqSA (VariableSA x) e, BtrueSA, [])
psp (phi,psi, SassumeSA e) = (BtrueSA, e, [])
psp (phi,psi, ScompSA s1 s2) = (mkAnd f1 f2, mkAnd psi1 psi2, v1++v2)
  where
    (f1,psi1,v1) = psp(phi,psi,s1)
    (f2,psi2,v2) = psp(mkAnd phi f1, mkAnd psi psi1, s2)
psp (phi,psi, SifSA b st sf) = (mkOr (mkAnd b ft) (mkAnd (BnegSA b) ff)
                              , mkOr (mkAnd b psit) (mkAnd (BnegSA b) psif)
                              , vt++vf)
  where
    (ft,psit,vt) = psp(phi,mkAnd psi b, st)
    (ff,psif,vf) = psp(phi,mkAnd psi (BnegSA b), sf)
psp (phi,psi, SassertSA e) = (BtrueSA, BtrueSA, [mkImpl (mkAnd phi psi) e])

pspplus :: (Expr, Expr, StmSA) -> (Expr, Expr, SetExpr)
pspplus (phi,psi, SskipSA) = (BtrueSA, BtrueSA, [])
pspplus (phi,psi, SassSA x e) = (BeqSA (VariableSA x) e, BtrueSA, [])
pspplus (phi,psi, SassumeSA e) = (BtrueSA, e, [])
pspplus (phi,psi, ScompSA s1 s2) = (mkAnd f1 f2, mkAnd psi1 psi2, v1++v2)
  where
    (f1,psi1,v1) = pspplus(phi,psi,s1)
    (f2,psi2,v2) = pspplus(mkAnd phi f1, mkAnd psi psi1, s2)
pspplus (phi,psi, SifSA b st sf) = (mkOr (mkAnd b ft) (mkAnd (BnegSA b) ff)
                              , mkOr (mkAnd b psit) (mkAnd (BnegSA b) psif)
                              , vt++vf)
  where
    (ft,psit,vt) = pspplus(phi,mkAnd psi b, st)
    (ff,psif,vf) = pspplus(phi,mkAnd psi (BnegSA b), sf)
pspplus (phi,psi, SassertSA e) = (BtrueSA, e, [mkImpl (mkAnd phi psi) e])

gsp :: (Expr, Expr, StmSA) -> (Expr, Expr, SetExpr)
gsp (phi,psi, SskipSA) = (BtrueSA, BtrueSA, [])
gsp (phi,psi, SassSA x e) = (BeqSA (VariableSA x) e, BtrueSA, [])
gsp (phi,psi, SassumeSA e) = (BtrueSA, e, [])
gsp (phi,psi, ScompSA s1 s2) = (mkAnd f1 f2, mkAnd psi1 psi2, v1++v2)
  where
    (f1,psi1,v1) = gsp(phi,psi,s1)
    (f2,psi2,v2) = gsp(mkAnd phi f1, mkAnd psi psi1, s2)
gsp (phi,psi, SifSA b st sf) = (mkOr (mkAnd b ft) (mkAnd (BnegSA b) ff)
                              , mkOr (mkAnd b psit) (mkAnd (BnegSA b) psif)
                              , vt++vf)
  where
    (ft,psit,vt) = gsp(phi,mkAnd psi b, st)
    (ff,psif,vf) = gsp(phi,mkAnd psi (BnegSA b), sf)
gsp (phi,psi, SassertSA e) = (BtrueSA, BtrueSA, [mkImpl psi e])

gspplus :: (Expr, Expr, StmSA) -> (Expr, Expr, SetExpr)
gspplus (phi,psi, SskipSA) = (BtrueSA, BtrueSA, [])
gspplus (phi,psi, SassSA x e) = (BeqSA (VariableSA x) e, BtrueSA, [])
gspplus (phi,psi, SassumeSA e) = (BtrueSA, e, [])
gspplus (phi,psi, ScompSA s1 s2) = (mkAnd f1 f2, mkAnd psi1 psi2, v1++v2)
  where
    (f1,psi1,v1) = gspplus(phi,psi,s1)
    (f2,psi2,v2) = gspplus(mkAnd phi f1, mkAnd psi psi1, s2)
gspplus (phi,psi, SifSA b st sf) = (mkOr (mkAnd b ft) (mkAnd (BnegSA b) ff)
                              , mkOr (mkAnd b psit) (mkAnd (BnegSA b) psif)
                              , vt++vf)
  where
    (ft,psit,vt) = gspplus(phi,mkAnd psi b, st)
    (ff,psif,vf) = gspplus(phi,mkAnd psi (BnegSA b), sf)
gspplus (phi,psi, SassertSA e) = (BtrueSA, e, [mkImpl psi e])