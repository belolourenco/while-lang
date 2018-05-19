module Language.Translations.TranslationFor where

import Data.Maybe
import Text.PrettyPrint

import Language.While.PrettyPrinter
import Language.WhileSA.PrettyPrinter
import Language.While.Utils.Utils
import Language.While.Types
import Language.WhileSA.Types
import Language.Translations.Base

-- * Translation for SA-For language
tsaFor :: Stm -> Versions -> (Maybe Versions,Maybe Versions,StmSA)
tsaFor (Scomp s1 s2) vs =
  let (vs1,vse1,s1') = tsaFor s1 vs
      (vs2,vse2,s2') = if isNothing vs1 then
                         error ("unreachable code after: " ++ (render.pretty $ s1'))
                       else tsaFor s2 (fromJust vs1)
  in (vs2
     , sup vse1 vse2
     , mkComp (mkMrgEx s1' vse1 vse2) (mkMrgEx s2' vse2 vse1))
tsaFor (Sif b s1 s2) vs =
  let b' = tsaBexp b vs
      (vs1,vse1,s1') = tsaFor s1 vs
      (vs2,vse2,s2') = tsaFor s2 vs
      s1sync = mkComp s1' (rnmToAssign $ merge vs1 vs2)
      s2sync = mkComp s2' (rnmToAssign $ merge vs2 vs1)
  in (sup vs1 vs2
     , sup vse1 vse2
     , SifSA b' (mkMrgEx s1sync vse1 vse2) (mkMrgEx s2sync vse2 vse1))
tsaFor (Swhile b s)  vs = tsaFor (SwhileInv b Btrue s) vs
tsaFor (SwhileInv b inv s) vs =
  let asgn_s = asgn s
      vs' = initVars asgn_s vs
      asgn_vs = filter (\(n,v) -> elem n asgn_s) vs'
      initVs = mkRnm asgn_vs vs
      b' = tsaBexp b vs'
      inv' = tsaBexp inv vs'
      (vs1,ve1,s1) = tsaFor s vs'
      updVs = if isNothing vs1 then
                error ("unreachable code after: " ++ (render.pretty $ s1))
              else mkRnm asgn_vs (fromJust vs1)
      all = ScompSA (SforInvSA initVs b' updVs inv' s1) (rnmToAssign $ upd asgn_vs)
      v'' = map (finalVersion asgn_vs) (fromJust vs1)
  in (Just v'', ve1, all)
  where
    initVars :: [Varname] -> Versions -> Versions
    initVars v vs = foldr newVar vs v

    mkRnm :: Versions -> Versions -> Rnm
    mkRnm v1 v2 = foldr (\(n,v) vs -> ((n,v),getVar n v2) : vs) [] v1

    finalVersion :: Versions -> VarnameSA -> VarnameSA
    finalVersion vs (n,v) = case lookup n vs of
                              Nothing -> (n,v)
                              Just v -> (n,jump v)
    
tsaFor (Stry s1 s2) vs = 
  let (vs1,vse1,s1') = tsaFor s1 vs
      (vs2,vse2,s2') = if isNothing vse1 then
                         error "unreachable catch statement"
                       else
                         tsaFor s2 (fromJust vse1)
  in (sup vs1 vs2
     , vse2
     , StrySA (mkComp s1' (rnmToAssign $ merge vs1 vs2))
       (mkComp s2' (rnmToAssign $ merge vs2 vs1)))
tsaFor Sthrow vs = (Nothing, Just vs, SthrowSA)
tsaFor s vs = tsa s vs

-- * Main function. Transforms a Stm into SA-for language (StmSA)
forLoopTrans :: Stm -> StmSA
forLoopTrans s = let (_,_,s') = tsaFor s (initV $ vars s)
                 in s'
