module Language.VCGens.LinGeneralization where

import Language.Logic.Types
import Language.WhileSA.Types

plin :: StmSA -> (Expr, Expr, Expr)
plin SskipSA         = (BtrueSA, BtrueSA, BtrueSA)
plin (SassSA x e)    = (BeqSA (VariableSA x) e, BtrueSA, BtrueSA)
plin (SassumeSA e)   = (BtrueSA, e, BtrueSA)
plin (ScompSA s1 s2) = (mkAnd psi1 psi2
                       , mkAnd gamma1 gamma2
                       , mkAnd omega1 (mkImpl (mkAnd psi1 gamma1) omega2))
  where (psi1,gamma1,omega1) = plin s1
        (psi2,gamma2,omega2) = plin s2
plin (SifSA b st sf) = (mkOr (mkAnd b psit) (mkAnd (mkNeg b) psif)
                       ,mkOr (mkAnd b gammat) (mkAnd (mkNeg b) gammaf)
                       ,mkAnd (mkImpl b omegat) (mkImpl (mkNeg b) omegaf)
                       )
  where (psit,gammat,omegat) = plin st
        (psif,gammaf,omegaf) = plin sf
plin (SassertSA e)   = (BtrueSA, BtrueSA, e)

plinplus :: StmSA -> (Expr, Expr, Expr)
plinplus SskipSA         = (BtrueSA, BtrueSA, BtrueSA)
plinplus (SassSA x e)    = (BeqSA (VariableSA x) e, BtrueSA, BtrueSA)
plinplus (SassumeSA e)   = (BtrueSA, e, BtrueSA)
plinplus (ScompSA s1 s2) = (mkAnd psi1 psi2
                           , mkAnd gamma1 gamma2
                           , mkAnd omega1 (mkImpl (mkAnd psi1 gamma1) omega2))
  where (psi1,gamma1,omega1) = plinplus s1
        (psi2,gamma2,omega2) = plinplus s2
plinplus (SifSA b st sf) = (mkOr (mkAnd b psit) (mkAnd (mkNeg b) psif)
                           ,mkOr (mkAnd b gammat) (mkAnd (mkNeg b) gammaf)
                           ,mkAnd (mkImpl b omegat) (mkImpl (mkNeg b) omegaf)
                           )
  where (psit,gammat,omegat) = plinplus st
        (psif,gammaf,omegaf) = plinplus sf
plinplus (SassertSA e)   = (BtrueSA, e, e)

glin :: StmSA -> (Expr, Expr, Expr)
glin SskipSA         = (BtrueSA, BtrueSA, BtrueSA)
glin (SassSA x e)    = (BeqSA (VariableSA x) e, BtrueSA, BtrueSA)
glin (SassumeSA e)   = (BtrueSA, e, BtrueSA)
glin (ScompSA s1 s2) = (mkAnd psi1 psi2
                       , mkAnd gamma1 gamma2
                       , mkAnd omega1 (mkImpl gamma1 omega2))
  where (psi1,gamma1,omega1) = glin s1
        (psi2,gamma2,omega2) = glin s2
glin (SifSA b st sf) = (mkOr (mkAnd b psit) (mkAnd (mkNeg b) psif)
                       ,mkOr (mkAnd b gammat) (mkAnd (mkNeg b) gammaf)
                       ,mkAnd (mkImpl b omegat) (mkImpl (mkNeg b) omegaf)
                       )
  where (psit,gammat,omegat) = glin st
        (psif,gammaf,omegaf) = glin sf
glin (SassertSA e)   = (BtrueSA, BtrueSA, e)

glinplus :: StmSA -> (Expr, Expr, Expr)
glinplus SskipSA         = (BtrueSA, BtrueSA, BtrueSA)
glinplus (SassSA x e)    = (BeqSA (VariableSA x) e, BtrueSA, BtrueSA)
glinplus (SassumeSA e)   = (BtrueSA,e, BtrueSA)
glinplus (ScompSA s1 s2) = (mkAnd psi1 psi2
                           , mkAnd gamma1 gamma2
                           , mkAnd omega1 (mkImpl gamma1 omega2))
  where (psi1,gamma1,omega1) = glinplus s1
        (psi2,gamma2,omega2) = glinplus s2
glinplus (SifSA b st sf) = (mkOr (mkAnd b psit) (mkAnd (mkNeg b) psif)
                           ,mkOr (mkAnd b gammat) (mkAnd (mkNeg b) gammaf)
                           ,mkAnd (mkImpl b omegat) (mkImpl (mkNeg b) omegaf)
                           )
  where (psit,gammat,omegat) = glinplus st
        (psif,gammaf,omegaf) = glinplus sf
glinplus (SassertSA e)   = (BtrueSA, e, e)