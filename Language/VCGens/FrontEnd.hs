module Language.VCGens.FrontEnd where

import Language.Logic.Types
import Language.WhileSA.Types
import Language.VCGens.Base
import Language.VCGens.SPGeneralization
import Language.VCGens.CNFGeneralization

data VCGen = PSP
           | PSPPlus
           | GSP
           | GSPPlus
           | PCNF
           | PCNFPlus
           | GCNF
           | GCNFPlus

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