module Language.VCGens.CNFGeneralization where

import Language.Logic.Types
import Language.WhileSA.Types
import Language.VCGens.Base

pcnf :: (Expr, Expr, Expr,StmSA) -> (Expr, Expr, SetExpr)
pcnf (pi,psi,gamma, SskipSA)        = (BtrueSA, BtrueSA, [])
pcnf (pi,psi,gamma, SassSA x e)    = (mkImpl pi (BeqSA (VariableSA x) e), BtrueSA, [])
pcnf (pi,psi,gamma, SassumeSA e)   = (BtrueSA, mkImpl pi e, [])
pcnf (pi,psi,gamma, ScompSA s1 s2) = (mkAnd psi1 psi2, mkAnd gamma1 gamma2, v1++v2)
  where (psi1,gamma1,v1) = pcnf (pi,psi,gamma,s1)
        (psi2,gamma2,v2) = pcnf (pi,mkAnd psi psi1, mkAnd gamma gamma1, s2)
pcnf (pi,psi,gamma, SifSA b st sf) = (mkAnd psit psif, mkAnd gammat gammaf, vt++vf)
  where (psit,gammat,vt) = pcnf (mkAnd pi b, psi,gamma,st)
        (psif,gammaf,vf) = pcnf (mkAnd pi (BnegSA b), psi,gamma,sf)
pcnf (pi,psi,gamma, SassertSA e)   = (BtrueSA, BtrueSA, [mkImpl (mkAnd psi gamma) (mkImpl pi e)])

pcnfplus :: (Expr, Expr, Expr,StmSA) -> (Expr, Expr, SetExpr)
pcnfplus (pi,psi,gamma, SskipSA)        = (BtrueSA, BtrueSA, [])
pcnfplus (pi,psi,gamma, SassSA x e)    = (mkImpl pi (BeqSA (VariableSA x) e), BtrueSA, [])
pcnfplus (pi,psi,gamma, SassumeSA e)   = (BtrueSA, mkImpl pi e, [])
pcnfplus (pi,psi,gamma, ScompSA s1 s2) = (mkAnd psi1 psi2, mkAnd gamma1 gamma2, v1++v2)
  where (psi1,gamma1,v1) = pcnfplus (pi,psi,gamma,s1)
        (psi2,gamma2,v2) = pcnfplus (pi,mkAnd psi psi1, mkAnd gamma gamma1, s2)
pcnfplus (pi,psi,gamma, SifSA b st sf) = (mkAnd psit psif, mkAnd gammat gammaf, vt++vf)
  where (psit,gammat,vt) = pcnfplus (mkAnd pi b, psi,gamma,st)
        (psif,gammaf,vf) = pcnfplus (mkAnd pi (BnegSA b), psi,gamma,sf)
pcnfplus (pi,psi,gamma, SassertSA e)   = (BtrueSA, mkImpl pi e, [mkImpl (mkAnd psi gamma) (mkImpl pi e)])

gcnf :: (Expr, Expr, Expr,StmSA) -> (Expr, Expr, SetExpr)
gcnf (pi,psi,gamma, SskipSA)        = (BtrueSA, BtrueSA, [])
gcnf (pi,psi,gamma, SassSA x e)    = (mkImpl pi (BeqSA (VariableSA x) e), BtrueSA, [])
gcnf (pi,psi,gamma, SassumeSA e)   = (BtrueSA, mkImpl pi e, [])
gcnf (pi,psi,gamma, ScompSA s1 s2) = (mkAnd psi1 psi2, mkAnd gamma1 gamma2, v1++v2)
  where (psi1,gamma1,v1) = gcnf (pi,psi,gamma,s1)
        (psi2,gamma2,v2) = gcnf (pi,mkAnd psi psi1, mkAnd gamma gamma1, s2)
gcnf (pi,psi,gamma, SifSA b st sf) = (mkAnd psit psif, mkAnd gammat gammaf, vt++vf)
  where (psit,gammat,vt) = gcnf (mkAnd pi b, psi,gamma,st)
        (psif,gammaf,vf) = gcnf (mkAnd pi (BnegSA b), psi,gamma,sf)
gcnf (pi,psi,gamma, SassertSA e)   = (BtrueSA, BtrueSA, [mkImpl gamma (mkImpl pi e)])

gcnfplus :: (Expr, Expr, Expr,StmSA) -> (Expr, Expr, SetExpr)
gcnfplus (pi,psi,gamma, SskipSA)        = (BtrueSA, BtrueSA, [])
gcnfplus (pi,psi,gamma, SassSA x e)    = (mkImpl pi (BeqSA (VariableSA x) e), BtrueSA, [])
gcnfplus (pi,psi,gamma, SassumeSA e)   = (BtrueSA, mkImpl pi e, [])
gcnfplus (pi,psi,gamma, ScompSA s1 s2) = (mkAnd psi1 psi2, mkAnd gamma1 gamma2, v1++v2)
  where (psi1,gamma1,v1) = gcnfplus (pi,psi,gamma,s1)
        (psi2,gamma2,v2) = gcnfplus (pi,mkAnd psi psi1, mkAnd gamma gamma1, s2)
gcnfplus (pi,psi,gamma, SifSA b st sf) = (mkAnd psit psif, mkAnd gammat gammaf, vt++vf)
  where (psit,gammat,vt) = gcnfplus (mkAnd pi b, psi,gamma,st)
        (psif,gammaf,vf) = gcnfplus (mkAnd pi (BnegSA b), psi,gamma,sf)
gcnfplus (pi,psi,gamma, SassertSA e)   = (BtrueSA, mkImpl pi e, [mkImpl gamma (mkImpl pi e)])