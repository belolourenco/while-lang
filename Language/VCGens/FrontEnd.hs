module Language.VCGens.FrontEnd where

import Language.Logic.Types
import Language.WhileSA.Types
import Language.VCGens.Base
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
           deriving Show
        
vcs :: StmSA -> VCGen -> SetExpr
vcs s PSP      = let (_,_,_,_,v) = sp Part AsrtNot (BtrueSA,BtrueSA,s) in v
vcs s PSPPlus  = let (_,_,_,_,v) = sp Part AsrtIn (BtrueSA,BtrueSA,s) in v
vcs s GSP      = let (phi,_,_,_,v) = sp Glob AsrtNot (BtrueSA,BtrueSA,s) 
                   in [mkImpl phi (mkBigAnd v)]
vcs s GSPPlus  = let (phi,_,_,_,v) = sp Glob AsrtIn (BtrueSA,BtrueSA,s) 
                   in [mkImpl phi (mkBigAnd v)]
vcs s PCNF     = let (_,_,_,_,v) = cnf Part AsrtNot (BtrueSA,BtrueSA,BtrueSA,s) in v
vcs s PCNFPlus = let (_,_,_,_,v) = cnf Part AsrtIn (BtrueSA,BtrueSA,BtrueSA,s) in v
vcs s GCNF     = let (psi,_,_,_,v) = cnf Glob AsrtNot (BtrueSA,BtrueSA,BtrueSA,s) 
                 in [mkImpl psi (mkBigAnd v)]
vcs s GCNFPlus = let (psi,_,_,_,v) = cnf Glob AsrtIn (BtrueSA,BtrueSA,BtrueSA,s) 
                 in [mkImpl psi (mkBigAnd v)]
vcs s PLin     = let (_,_,_,_,v) = lin Part AsrtNot s in [v]
vcs s PLinPlus = let (_,_,_,_,v) = lin Part AsrtIn s in [v]
vcs s GLin     = let (psi,_,_,_,v) = lin Glob AsrtNot s 
                 in [mkImpl psi v]
vcs s GLinPlus = let (psi,_,_,_,v) = lin Glob AsrtIn s 
                 in [mkImpl psi v]

-- This is just a temporary solution. See omnigraffle diagram for a cleaner solution
vcs_iter :: StmSA -> SetExpr
vcs_iter s = let (psi, gamma, t) = lin_iter s in (t ++ [gamma] ++ [psi])
