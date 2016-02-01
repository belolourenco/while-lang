module Language.VCGens.Linear_Iter where

import Language.Logic.Types
import Language.WhileSA.Types
import Data.Maybe

-- This is just a temporary solution. See omnigraffle diagram for a cleaner solution

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

lin_iter :: StmSA -> (Expr, Expr, [Expr])
lin_iter SskipSA                = (BtrueSA, BtrueSA, [])
lin_iter (SassSA x e)           = (BeqSA (VariableSA x) e, BtrueSA, [])
lin_iter (ScompSA s1 s2)        = (mkAnd psi1 psi2
                                  , mkAnd gamma1 (mkImpl psi1 gamma2)
                                  , t1 ++ t2)
  where (psi1,gamma1,t1) = lin_iter s1
        (psi2,gamma2,t2) = lin_iter s2
lin_iter (SifSA b st sf)        = (mkOr (mkAnd b psit) (mkAnd (mkNeg b) psif)
                                  , mkAnd (mkImpl b gammat) (mkImpl (mkNeg b) gammaf)
                                  , tt ++ tf)
  where (psit,gammat,tt) = lin_iter st
        (psif,gammaf,tf) = lin_iter sf
lin_iter (SforInvSA i b u inv s) = (mkAnd inv (mkNeg b)
                                   , applyRnm i inv
                                   , [mkImpl (mkAnd inv b) 
                                             (mkAnd gamma 
                                                    (mkImpl psi (applyRnm u inv)))] ++ t)
  where (psi,gamma,t)    = lin_iter s