module Language.Translations.TranslationHavoc where

import Data.Maybe
import Data.List
import Text.PrettyPrint

import Language.While.PrettyPrinter
import Language.While.Utils.Utils
import Language.While.Types
import Language.WhileSA.Types
import Language.Translations.Base
import Language.Translations.TranslationFor

-- Two versions for the translation. One detects dead code in
-- try-catch statements and the other does not!

-- * Translation for SA-LoopFree language
-- This translation detects dead code and fails if there is a catch without a try
tsaLF1 :: Stm -> Versions -> (Maybe Versions,Maybe Versions,StmSA)
tsaLF1 (Scomp s1 s2) vs =
  let (vs1,vse1,s1') = tsaLF1 s1 vs
      (vs2,vse2,s2') = if isNothing vs1 then
                         error ("unreachable code after: " ++ (render.pretty $ s1'))
                       else tsaLF1 s2 (fromJust vs1)
  in (vs2
     , sup vse1 vse2
     , mkComp (mkMrgEx s1' vse1 vse2) (mkMrgEx s2' vse2 vse1))
tsaLF1 (Sif b s1 s2) vs =
  let b' = tsaBexp b vs
      (vs1,vse1,s1') = tsaLF1 s1 vs
      (vs2,vse2,s2') = tsaLF1 s2 vs
      s1sync = mkComp s1' (rnmToAssign $ merge vs1 vs2)
      s2sync = mkComp s2' (rnmToAssign $ merge vs2 vs1)
  in (sup vs1 vs2
     , sup vse1 vse2
     , SifSA b' (mkMrgEx s1sync vse1 vse2) (mkMrgEx s2sync vse2 vse1))
tsaLF1 (Swhile b s) vs = error "Loop invariants are mandatory for this translation to proceed"
tsaLF1 (SwhileInv b inv s) vs =
  error "while loops not supported yet"
-- do
--  let asgn_s = asgn s
--  assertBefore <- tsaBexp inv >>= return.SassertSA
--  incVars asgn_s
--  assumeBefore <- tsaBexp (Band b inv) >>= return.SassumeSA
--  s' <- tsaLF1 s
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
tsaLF1 (Stry s1 s2) vs = 
  let (vs1,vse1,s1') = tsaLF1 s1 vs
      (vs2,vse2,s2') = if isNothing vse1 then
                         error "unreachable catch statement"
                       else
                         tsaLF1 s2 (fromJust vse1)
  in (sup vs1 vs2
     , vse2
     , StrySA (mkComp s1' (rnmToAssign $ merge vs1 vs2))
       (mkComp s2' (rnmToAssign $ merge vs2 vs1)))
tsaLF1 Sthrow vs = (Nothing, Just vs, SthrowSA)
tsaLF1 s vs = tsa s vs

-- | It creates a Rnm, whose domain contains the variables from the first Version
-- list that are smaller than the version in the second list
mrg :: Versions -> Versions -> Rnm
mrg v1 v2 = map aux2 $ filter aux v1
  where
    aux :: VarnameSA -> Bool
    aux x = (snd x) < (fromJust $ lookup (fst x) v2)

    aux2 :: VarnameSA -> (VarnameSA, VarnameSA)
    aux2 x = ((fst x, fromJust $ lookup (fst x) v2), x)

sync :: StmSA -> StmSA -> Versions -> Versions -> (StmSA,StmSA)
sync c1 c2 v1 v2 = (c1',c2')
  where c1' = case mrg v1 v2 of
                [] -> c1
                r  -> StrySA c1 (mkComp (rnmToAssign r) SthrowSA)

        c2' = case mrg v2 v1 of
                [] -> c2
                r -> StrySA c2 (mkComp (rnmToAssign r) SthrowSA)

-- | Merges two Version lists, choosing for each variable the highest Version
sup' :: Versions -> Versions -> Versions
sup'  v1 v2 =
  map aux $ nub $ (map fst v1) ++ (map fst v2)
  where
    aux x = (x, greater (lookup x v1) (lookup x v2))

    greater :: Maybe Version -> Maybe Version -> Version
    greater Nothing v           = fromJust v
    greater v Nothing           = fromJust v
    greater (Just v1) (Just v2) = if (head v1) > (head v2) then v1 else v2 


-- * Translation for SA-LoopFree language
-- This translation does not detect dead code
tsaLF2 :: Stm -> Versions -> (Versions,Versions,StmSA)
tsaLF2 Sthrow        vs = (vs, vs, SthrowSA)
tsaLF2 Sskip         vs = (vs, vs, SskipSA)
tsaLF2 (Sassume b)   vs = (vs, vs, SassumeSA $ tsaBexp b vs)
tsaLF2 (Sassert b)   vs = (vs, vs, SassertSA $ tsaBexp b vs)
tsaLF2 (Sass n e)    vs = let vs' = nextVar n vs
                              asgn = SassSA (getVar n vs') (tsaAexp e vs)
                          in (vs',vs, asgn)
tsaLF2 (Scomp s1 s2) vs =
  let (vs1,vse1,s1') = tsaLF2 s1 vs
      (vs2,vse2,s2') = tsaLF2 s2 vs1
      (s1'',s2'') = sync s1' s2' vse1 vse2
  in (vs2
     , sup' vse1 vse2
     , mkComp s1'' s2'')
tsaLF2 (Stry s1 s2) vs = 
  let (vs1,vse1,s1') = tsaLF2 s1 vs
      (vs2,vse2,s2') = tsaLF2 s2 vse1
  in (sup' vs1 vs2
     , vse2
     , StrySA (mkComp s1' (rnmToAssign $ mrg vs1 vs2))
       (mkComp s2' (rnmToAssign $ mrg vs2 vs1)))
tsaLF2 (Sif b s1 s2) vs =
  let b' = tsaBexp b vs
      (vs1,vse1,s1') = tsaLF2 s1 vs
      (vs2,vse2,s2') = tsaLF2 s2 vs
      (s1'',s2'') = sync (mkComp s1' (rnmToAssign $ mrg vs1 vs2))
                         (mkComp s2' (rnmToAssign $ mrg vs2 vs1))
                         vse1
                         vse2
  in (sup' vs1 vs2
     , sup' vse1 vse2
     , SifSA b' s1'' s2'')
-- tsaLF2 (Swhile b s) vs = error "Loop invariants are mandatory for this translation to proceed"
-- tsaLF2 (SwhileInv b inv s) vs =
--   error "while loops not supported yet"
-- -- do
-- --  let asgn_s = asgn s
-- --  assertBefore <- tsaBexp inv >>= return.SassertSA
-- --  incVars asgn_s
-- --  assumeBefore <- tsaBexp (Band b inv) >>= return.SassumeSA
-- --  s' <- tsaLF2 s
-- --  assertAfter <- tsaBexp inv >>= return.SassertSA
-- --  incVars asgn_s
-- --  assumeAfter <- tsaBexp (Band inv (Bneg b)) >>= return.SassumeSA
-- --  return $ ScompSA assertBefore 
-- --                   (ScompSA assumeBefore 
-- --                            (ScompSA s' 
-- --                                    (ScompSA assertAfter 
-- --                                             assumeAfter)))
-- --  where
-- --    incVars :: [Varname] -> State Versions ()
-- --    incVars []     = return ()
-- --    incVars (n:ns) = nextVar n >> incVars ns

-- * Main function. Transforms a Stm into SA-LoopFree language (StmSA)
havocTrans :: Int -> Stm -> StmSA
havocTrans 1 s = let (_,_,s') = tsaLF1 s (initV $ vars s)
                 in s'
havocTrans _ s = let (_,_,s') =  tsaLF2 s (initV $ vars s)
                 in s'
