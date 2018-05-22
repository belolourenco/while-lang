module Language.VCGens.FrontEnd where

import Language.Logic.Types
import Language.WhileSA.Types
import Language.VCGens.Base
import Language.VCGens.SPGeneralization
import Language.VCGens.CNFGeneralization
import Language.VCGens.LinGeneralization
import Language.VCGens.Linear_Iter

generate :: StmSA -> VCGen -> VOp -> [LExpr]
generate s SP VCP   = let (_,_,_,_,v) = sp Part AsrtNot (BtrueSA,BtrueSA,s) in v
generate s SP VCPA  = let (_,_,_,_,v) = sp Part AsrtIn (BtrueSA,BtrueSA,s) in v
generate s SP VCG   = let (phi,_,_,_,v) = sp Glob AsrtNot (BtrueSA,BtrueSA,s) 
                      in [mkImpl phi (mkBigAnd v)]
generate s SP VCGA  = let (phi,_,_,_,v) = sp Glob AsrtIn (BtrueSA,BtrueSA,s) 
                      in [mkImpl phi (mkBigAnd v)]
generate s CNF VCP  = let (_,_,_,_,v) = cnf Part AsrtNot (BtrueSA,BtrueSA,BtrueSA,s) in v
generate s CNF VCPA = let (_,_,_,_,v) = cnf Part AsrtIn (BtrueSA,BtrueSA,BtrueSA,s) in v
generate s CNF VCG  = let (psi,_,_,_,v) = cnf Glob AsrtNot (BtrueSA,BtrueSA,BtrueSA,s) 
                      in [mkImpl psi (mkBigAnd v)]
generate s CNF VCGA = let (psi,_,_,_,v) = cnf Glob AsrtIn (BtrueSA,BtrueSA,BtrueSA,s) 
                      in [mkImpl psi (mkBigAnd v)]
generate s LIN VCP  = let (_,_,_,_,v) = lin Part AsrtNot s in [v]
generate s LIN VCPA = let (_,_,_,_,v) = lin Part AsrtIn s in [v]
generate s LIN VCG  = let (psi,_,_,_,v) = lin Glob AsrtNot s 
                      in [mkImpl psi v]
generate s LIN VCGA = let (psi,_,_,_,v) = lin Glob AsrtIn s 
                      in [mkImpl psi v]

-- This is just a temporary solution. See omnigraffle diagram for a cleaner solution
vcs_iter :: StmSA -> [LExpr]
vcs_iter s = let (psi, gamma, t) = lin_iter s in (t ++ [gamma] ++ [psi])
