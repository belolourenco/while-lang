module Language.Translations.TranslationHavoc where

import Data.Maybe
import Text.PrettyPrint

import Language.While.PrettyPrinter
import Language.While.Utils.Utils
import Language.While.Types
import Language.WhileSA.Types
import Language.Translations.Base
import Language.Translations.TranslationFor

-- * Translation for SA-LoopFree language
tsaLF :: Stm -> Versions -> (Maybe Versions,Maybe Versions,StmSA)
tsaLF (Scomp s1 s2) vs =
  let (vs1,vse1,s1') = tsaLF s1 vs
      (vs2,vse2,s2') = if isNothing vs1 then
                         error ("unreachable code after: " ++ (render.pretty $ s1'))
                       else tsaLF s2 (fromJust vs1)
  in (vs2
     , sup vse1 vse2
     , mkComp (mkMrgEx s1' vse1 vse2) (mkMrgEx s2' vse2 vse1))
tsaLF (Sif b s1 s2) vs =
  let b' = tsaBexp b vs
      (vs1,vse1,s1') = tsaLF s1 vs
      (vs2,vse2,s2') = tsaLF s2 vs
      s1sync = mkComp s1' (rnmToAssign $ merge vs1 vs2)
      s2sync = mkComp s2' (rnmToAssign $ merge vs2 vs1)
  in (sup vs1 vs2
     , sup vse1 vse2
     , SifSA b' (mkMrgEx s1sync vse1 vse2) (mkMrgEx s2sync vse2 vse1))
tsaLF (Swhile b s) vs = error "Loop invariants are mandatory for this translation to proceed"
tsaLF (SwhileInv b inv s) vs =
  error "while loops not supported yet"
-- do
--  let asgn_s = asgn s
--  assertBefore <- tsaBexp inv >>= return.SassertSA
--  incVars asgn_s
--  assumeBefore <- tsaBexp (Band b inv) >>= return.SassumeSA
--  s' <- tsaLF s
--  assertAfter <- tsaBexp inv >>= return.SassertSA
--  incVars asgn_s
--  assumeAfter <- tsaBexp (Band inv (Bneg b)) >>= return.SassumeSA
--  return $ ScompSA assertBefore 
--                   (ScompSA assumeBefore 
--                            (ScompSA s' 
--                                    (ScompSA assertAfter 
--                                             assumeAfter)))
--  where
--    incVars :: [Varname] -> State Versions ()
--    incVars []     = return ()
--    incVars (n:ns) = nextVar n >> incVars ns
tsaLF (Stry s1 s2) vs = 
  let (vs1,vse1,s1') = tsaLF s1 vs
      (vs2,vse2,s2') = if isNothing vse1 then
                         error "unreachable catch statement"
                       else
                         tsaLF s2 (fromJust vse1)
  in (sup vs1 vs2
     , vse2
     , StrySA (mkComp s1' (rnmToAssign $ merge vs1 vs2))
       (mkComp s2' (rnmToAssign $ merge vs2 vs1)))
tsaLF Sthrow vs = (Nothing, Just vs, SthrowSA)
tsaLF s vs = tsa s vs

-- * Main function. Transforms a Stm into SA-LoopFree language (StmSA)
havocTrans :: Stm -> StmSA
havocTrans s = let (_,_,s') = tsaLF s (initV $ vars s)
               in s'
