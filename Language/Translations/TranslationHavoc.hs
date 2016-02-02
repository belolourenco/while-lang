module Language.Translations.TranslationHavoc where

import Control.Monad.State

import Language.While.Utils.Utils
import Language.While.Types
import Language.WhileSA.Types
import Language.Translations.Base
import Language.Translations.TranslationFor

-- * Translation for SA-LoopFree language
tsaLF :: Stm -> State VersionList StmSA
tsaLF (Scomp s1 s2) = tsaLF s1 >>= \s1' -> tsaLF s2
                               >>= \s2' -> return $ ScompSA s1' s2'
tsaLF (Sif b s1 s2) = do
  v <- get
  b'  <- tsaBexp b
  s1' <- tsaLF s1
  v' <- get
  put v
  s2' <- tsaLF s2
  v'' <- get
  put $ sup v' v''
  let mergeInThen = (rnmToAssign $ merge v' v'')
  let mergeInElse = (rnmToAssign $ merge v'' v')
  return $ SifSA b' (if mergeInThen == SskipSA then s1' else ScompSA s1' mergeInThen)
                    (if mergeInElse == SskipSA then s2' else ScompSA s2' mergeInElse)
tsaLF (Swhile b s) = error "Loop invariants are mandatory for this translation to proceed"
tsaLF (SwhileInv b inv s) = do
  let asgn_s = asgn s
  assertBefore <- tsaBexp inv >>= return.SassertSA
  incVars asgn_s
  assumeBefore <- tsaBexp (Band b inv) >>= return.SassumeSA
  s' <- tsaLF s
  assertAfter <- tsaBexp inv >>= return.SassertSA
  incVars asgn_s
  assumeAfter <- tsaBexp (Band inv (Bneg b)) >>= return.SassumeSA
  return $ ScompSA assertBefore 
                   (ScompSA assumeBefore 
                            (ScompSA s' 
                                    (ScompSA assertAfter 
                                             assumeAfter)))
  where
    incVars :: [Varname] -> State VersionList ()
    incVars []     = return ()
    incVars (n:ns) = nextVar n >> incVars ns
tsaLF s = tsa s
--tsa (Stry Stm Stm

-- * Main function. Transforms a Stm into SA-LoopFree language (StmSA)
havocTrans :: Stm -> StmSA
havocTrans s = evalState (tsaLF s) (initV $ vars s)