module Language.VCGens.FrontEnd where

import Language.Logic.Types
import Language.WhileSA.Types
import Language.VCGens.Base
import Language.VCGens.SPGeneralization
import Language.VCGens.CNFGeneralization
import Language.VCGens.LinGeneralization

data VCGen = SP | CNF | LIN

generate :: StmSA -> VCGen -> VOp -> [LExpr]
generate s SP  op | op == VCP || op == VCPA =
  let (_,_,_,_,v) = sp op (BtrueSA,BtrueSA,s) in v
generate s SP  op | op == VCG || op == VCGA =
  let (phi,_,_,_,v) = sp op (BtrueSA,BtrueSA,s) 
  in [mkImpl phi (mkBigAnd v)]
generate s CNF op | op == VCP || op == VCPA =
  let (_,_,_,_,v) = cnf op (BtrueSA,BtrueSA,BtrueSA,s) in v
generate s CNF op | op == VCG || op == VCGA  =
  let (psi,_,_,_,v) = cnf op (BtrueSA,BtrueSA,BtrueSA,s) 
  in [mkImpl psi (mkBigAnd v)]
generate s LIN op | op == VCP || op == VCPA =
  let (_,_,_,_,v) = lin op s in [v]
generate s LIN op | op == VCG || op == VCGA =
  let (psi,_,_,_,v) = lin op s 
  in [mkImpl psi v]
