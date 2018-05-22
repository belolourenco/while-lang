module Language.Translations.TranslationHavoc where

import Data.Maybe
import Data.List
import Text.PrettyPrint

import Language.While.PrettyPrinter
import Language.WhileSA.PrettyPrinter
import Language.While.Utils.Utils
import Language.While.Types
import Language.WhileSA.Types
import Language.Translations.Base

-- * Translation for SA-LoopFree language
-- This translation detects dead code and fails if there is a catch without a try
tsaHavoc :: Stm -> Versions -> (Maybe Versions,Maybe Versions,StmSA)
tsaHavoc (Scomp s1 s2) vs =
  let (vs1,vse1,s1') = tsaHavoc s1 vs
      (vs2,vse2,s2') = if isNothing vs1 then
                         error ("unreachable code after: " ++ (render.pretty $ s1'))
                       else tsaHavoc s2 (fromJust vs1)
  in (vs2
     , sup vse1 vse2
     , mkComp (mkMrgEx s1' vse1 vse2) (mkMrgEx s2' vse2 vse1))
tsaHavoc (Sif b s1 s2) vs =
  let b' = tsaBexp b vs
      (vs1,vse1,s1') = tsaHavoc s1 vs
      (vs2,vse2,s2') = tsaHavoc s2 vs
      s1sync = mkComp s1' (rnmToAssign $ merge vs1 vs2)
      s2sync = mkComp s2' (rnmToAssign $ merge vs2 vs1)
  in (sup vs1 vs2
     , sup vse1 vse2
     , SifSA b' (mkMrgEx s1sync vse1 vse2) (mkMrgEx s2sync vse2 vse1))
tsaHavoc (Swhile b s) vs = tsaHavoc (SwhileInv b Btrue s) vs
tsaHavoc (SwhileInv b inv s) vs =
  let asgn_s = asgn s
      assertInit = SassertSA (tsaBexp inv vs)
      vs' = incVars asgn_s vs
      assumeInit = SassumeSA (tsaBexp inv vs')
      (vs1,vse1,s1) = tsaHavoc s vs'
      assertAfter = if isNothing vs1 then SassertSA BtrueSA
                    else SassertSA (tsaBexp inv (fromJust vs1))
      assumeFalse = SassumeSA BfalseSA
      thenBody = ScompSA s1 (ScompSA assertAfter assumeFalse)
      elseBody = if isNothing vs1 then SskipSA
                 else rnmToAssign $ merge (Just vs') vs1
      ifStm = SifSA (tsaBexp b vs') thenBody elseBody
      all = ScompSA assertInit (ScompSA assumeInit  ifStm) in
    (vs1,vse1,all)
  where
    incVars :: [Varname] -> Versions -> Versions
    incVars v vs = foldr nextVar vs v  
tsaHavoc (Stry s1 s2) vs = 
  let (vs1,vse1,s1') = tsaHavoc s1 vs
      (vs2,vse2,s2') = if isNothing vse1 then
                         error "unreachable catch statement"
                       else
                         tsaHavoc s2 (fromJust vse1)
  in (sup vs1 vs2
     , vse2
     , StrySA (mkComp s1' (rnmToAssign $ merge vs1 vs2))
       (mkComp s2' (rnmToAssign $ merge vs2 vs1)))
tsaHavoc Sthrow vs = (Nothing, Just vs, SthrowSA)
tsaHavoc s vs = tsa s vs

-- * Main function. Transforms a Stm into SA-LoopFree language (StmSA)
-- havocTrans True s detects dead code, while havocTrans False s does not
havocTrans :: Stm -> StmSA
havocTrans s = let (_,_,s') = tsaHavoc s (initV $ vars s)
               in s'
