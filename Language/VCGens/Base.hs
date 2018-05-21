module Language.VCGens.Base where

data Context = Glob
             | Part

data AsrtCtx = AsrtIn
               | AsrtNot

data VOp = VCP | VCPA | VCG | VCGA

data VCGen = SP | CNF | LIN
           deriving Show
