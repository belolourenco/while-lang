module Language.VCGens.Base where

import Language.Logic.Types
import Language.WhileSA.Types

data VOp = VCP | VCPA | VCG | VCGA
  deriving Eq

applyRnmA :: Rnm -> AexpSA -> AexpSA
applyRnmA r (NumeralSA i)   = NumeralSA i
applyRnmA r (VariableSA n)  = case lookup n r of
                                Nothing -> VariableSA n
                                Just n' -> VariableSA n'
applyRnmA r (AaddSA e1 e2)  = AaddSA (applyRnmA r e1) (applyRnmA r e2)
applyRnmA r (AsubSA e1 e2)  = AsubSA (applyRnmA r e1) (applyRnmA r e2)
applyRnmA r (AmulSA e1 e2)  = AmulSA (applyRnmA r e1) (applyRnmA r e2)
applyRnmA r (AdivSA e1 e2)  = AdivSA (applyRnmA r e1) (applyRnmA r e2)

applyRnm :: Rnm -> BexpSA -> BexpSA
applyRnm r BtrueSA          = BtrueSA
applyRnm r BfalseSA         = BfalseSA
applyRnm r (BVariableSA n)  = case lookup n r of
                                Nothing -> BVariableSA n
                                Just n' -> BVariableSA n'
applyRnm r (BeqSA e1 e2)    = BeqSA (applyRnmA r e1) (applyRnmA r e2)
applyRnm r (BleqSA e1 e2)   = BleqSA (applyRnmA r e1) (applyRnmA r e2)
applyRnm r (BlSA e1 e2)     = BlSA (applyRnmA r e1) (applyRnmA r e2)
applyRnm r (BgSA e1 e2)     = BgSA (applyRnmA r e1) (applyRnmA r e2)
applyRnm r (BgeqSA e1 e2)   = BgeqSA (applyRnmA r e1) (applyRnmA r e2)
applyRnm r (BnegSA b)       = BnegSA (applyRnm r b)
applyRnm r (BandSA b1 b2)   = BandSA (applyRnm r b1) (applyRnm r b2)
applyRnm r (BorSA b1 b2)    = BorSA (applyRnm r b1) (applyRnm r b2)
applyRnm r (BimplSA b1 b2)  = BimplSA (applyRnm r b1) (applyRnm r b2)
