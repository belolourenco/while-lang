module Language.VCGens.FrontEnd where

import Language.Logic.Types
import Language.WhileSA.Types
import Language.VCGens.SPGeneralization
import Language.VCGens.CNFGeneralization
import Language.VCGens.LinGeneralization
import Language.VCGens.Linear_Iter

data VCGen = PSP
           | PSPPlus
           | GSP
           | GSPPlus
           | PCNF
           | PCNFPlus
           | GCNF
           | GCNFPlus
           | PLin
           | PLinPlus
           | GLin
           | GLinPlus

vcs :: StmSA -> VCGen -> SetExpr
vcs s PSP      = let (phi,psi,v) = psp (BtrueSA,BtrueSA,s) in v
vcs s PSPPlus  = let (phi,psi,v) = pspplus (BtrueSA,BtrueSA,s) in v
vcs s GSP      = let (phi,psi,v) = gsp (BtrueSA,BtrueSA,s) 
                   in [mkImpl phi (mkBigAnd v)]
vcs s GSPPlus  = let (phi,psi,v) = gspplus (BtrueSA,BtrueSA,s) 
                   in [mkImpl phi (mkBigAnd v)]
vcs s PCNF     = let (psi,gamma,v) = pcnf (BtrueSA,BtrueSA,BtrueSA,s) in v
vcs s PCNFPlus = let (psi,gamma,v) = pcnfplus (BtrueSA,BtrueSA,BtrueSA,s) in v
vcs s GCNF     = let (psi,gamma,v) = gcnf (BtrueSA,BtrueSA,BtrueSA,s) 
                 in [mkImpl psi (mkBigAnd v)]
vcs s GCNFPlus = let (psi,gamma,v) = gcnfplus (BtrueSA,BtrueSA,BtrueSA,s) 
                 in [mkImpl psi (mkBigAnd v)]
vcs s PLin     = let (psi,gamma,v) = plin s in [v]
vcs s PLinPlus = let (psi,gamma,v) = plinplus s in [v]
vcs s GLin     = let (psi,gamma,v) = glin s 
                 in [mkImpl psi v]
vcs s GLinPlus = let (psi,gamma,v) = glinplus s 
                 in [mkImpl psi v]

-- This is just a temporary solution. See omnigraffle diagram for a cleaner solution
vcs_iter :: StmSA -> SetExpr
vcs_iter s = let (psi, gamma, t) = lin_iter s in (t ++ [gamma] ++ [psi])
