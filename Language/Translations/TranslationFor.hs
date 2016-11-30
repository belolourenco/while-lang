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
tsaAexp :: Aexp -> Versions -> AexpSA
tsaAexp (Numeral i) _ = NumeralSA i
tsaAexp (Variable n) vs = VariableSA $ getVar n vs
tsaAexp (Aadd e1 e2) vs = AaddSA (tsaAexp e1 vs) (tsaAexp e2 vs)
tsaAexp (Asub e1 e2) vs = AsubSA (tsaAexp e1 vs) (tsaAexp e2 vs)
tsaAexp (Amul e1 e2) vs = AmulSA (tsaAexp e1 vs) (tsaAexp e2 vs)
tsaAexp (Adiv e1 e2) vs = AdivSA (tsaAexp e1 vs) (tsaAexp e2 vs)

tsaBexp :: Bexp -> Versions -> BexpSA
tsaBexp Btrue        vs = BtrueSA
tsaBexp Bfalse       vs = BfalseSA
tsaBexp (BVariable n)vs = BVariableSA (getVar n vs)
tsaBexp (Beq e1 e2)  vs = BeqSA  (tsaAexp e1 vs) (tsaAexp e2 vs)
tsaBexp (Bleq e1 e2) vs = BleqSA (tsaAexp e1 vs) (tsaAexp e2 vs)
tsaBexp (Bl e1 e2)   vs = BlSA   (tsaAexp e1 vs) (tsaAexp e2 vs)
tsaBexp (Bg e1 e2)   vs = BgSA   (tsaAexp e1 vs) (tsaAexp e2 vs)
tsaBexp (Bgeq e1 e2) vs = BgeqSA (tsaAexp e1 vs) (tsaAexp e2 vs)
tsaBexp (Bneg b)     vs = BnegSA (tsaBexp b vs)
tsaBexp (Band b1 b2) vs = BandSA (tsaBexp b1 vs) (tsaBexp b2 vs)
tsaBexp (Bor b1 b2)  vs = BorSA  (tsaBexp b1 vs) (tsaBexp b2 vs)
tsaBexp (Bimpl b1 b2)vs = BimplSA(tsaBexp b1 vs) (tsaBexp b2 vs)

mkComp :: StmSA -> StmSA -> StmSA
mkComp SskipSA s = s
mkComp s SskipSA = s
mkComp s1 s2 = ScompSA s1 s2

mkMrgEx :: StmSA -> Maybe Versions -> Maybe Versions -> StmSA
mkMrgEx s Nothing _ = s
mkMrgEx s _ Nothing = s
mkMrgEx s vs1 vs2   =
  StrySA s (mkComp (rnmToAssign $ merge vs1 vs2) SthrowSA)

tsa :: Stm -> Versions -> (Maybe Versions,Maybe Versions,StmSA)
tsa (Sass n e)    vs = let vs' = nextVar n vs
                           asgn = SassSA (getVar n vs') (tsaAexp e vs)
                       in (Just vs',Nothing, asgn)
tsa Sskip         vs  = (Just vs,Nothing,SskipSA)
tsa (Sassume b)   vs = (Just vs, Nothing, SassumeSA $ tsaBexp b vs)
tsa (Sassert b)   vs = (Just vs, Nothing, SassertSA $ tsaBexp b vs)
tsa (Scomp s1 s2) vs =
  let (vs1,vse1,s1') = tsa s1 vs
      (vs2,vse2,s2') = if isNothing vs1 then
                         error ("unreachable code after: " ++ (render.pretty $ s1'))
                       else tsa s2 (fromJust vs1)
  in (vs2
     , sup vse1 vse2
     , mkComp (mkMrgEx s1' vse1 vse2) (mkMrgEx s2' vse2 vse1))
tsa (Sif b s1 s2) vs =
  let b' = tsaBexp b vs
      (vs1,vse1,s1') = tsa s1 vs
      (vs2,vse2,s2') = tsa s2 vs
      s1sync = mkComp s1' (rnmToAssign $ merge vs1 vs2)
      s2sync = mkComp s2' (rnmToAssign $ merge vs2 vs1)
  in (sup vs1 vs2
     , sup vse1 vse2
     , SifSA b' (mkMrgEx s1sync vse1 vse2) (mkMrgEx s2sync vse2 vse1))
tsa (Swhile b s)  vs =
  error "while loops not supported yet"
--  do
--  let asgn_c = asgn s
--  v <- get
--  i <- create_i asgn_c
--  v' <- upd_v' asgn_c
--  b' <- tsaBexp b
--  s' <- tsa s
--  u <- create_u asgn_c v
--  let dom_u = map fst u
--  mapM_ (\x -> replacesVar (fst x) (jump $ snd x)) dom_u
--  return $ ScompSA  (SforSA i b' u s')
--                    (rnmToAssign $ upd dom_u)
--  where
--    create_i :: [Varname] -> State Versions Rnm
--    create_i []     = return []
--    create_i (n:ns) = getVarVer n >>= \v -> create_i ns 
--                                  >>= \ns' -> return $ ((n,new v), (n, v)):ns'
--
--    upd_v' :: [Varname] -> State Versions Versions
--    upd_v' []     = get >>= return
--    upd_v' (n:ns) = newVar n >> upd_v' ns
--
--    create_u :: [Varname] -> Versions -> State Versions Rnm
--    create_u []     vl = return []
--    create_u (n:ns) vl = getVarVer n >>= \v' -> create_u ns vl
--                                     >>= \ns' -> return $ ((n, new $ getVarVerAux n vl),(n,v')):ns'
tsa (SwhileInv b inv s) vs =
  error "while loops not supported yet"
--
--  do
--  let asgn_c = asgn s
--  v <- get
--  i <- create_i asgn_c
--  v' <- upd_v' asgn_c
--  b' <- tsaBexp b
--  inv' <- tsaBexp inv
--  s' <- tsa s
--  u <- create_u asgn_c v
--  let dom_u = map fst u
--  mapM_ (\x -> replacesVar (fst x) (jump $ snd x)) dom_u
--  return $ ScompSA  (SforInvSA i b' u inv' s')
--                    (rnmToAssign $ upd dom_u)
--  where
--    create_i :: [Varname] -> State Versions Rnm
--    create_i []     = return []
--    create_i (n:ns) = getVarVer n >>= \v -> create_i ns 
--                                  >>= \ns' -> return $ ((n,new v), (n, v)):ns'
--
--    upd_v' :: [Varname] -> State Versions Versions
--    upd_v' []     = get >>= return
--    upd_v' (n:ns) = newVar n >> upd_v' ns
--
--    create_u :: [Varname] -> Versions -> State Versions Rnm
--    create_u []     vl = return []
--    create_u (n:ns) vl = getVarVer n >>= \v' -> create_u ns vl
--                                     >>= \ns' -> return $ ((n, new $ getVarVerAux n vl),(n,v')):ns'
tsa (Stry s1 s2) vs = 
  let (vs1,vse1,s1') = tsa s1 vs
      (vs2,vse2,s2') = if isNothing vse1 then
                         error "unreachable catch statement"
                       else
                         tsa s2 (fromJust vse1)
  in (sup vs1 vs2
     , vse2
     , StrySA (mkComp s1' (rnmToAssign $ merge vs1 vs2))
       (mkComp s2' (rnmToAssign $ merge vs2 vs1)))
tsa Sthrow vs = (Nothing, Just vs, SthrowSA)

-- * Main function. Transforms a Stm into SA-for language (StmSA)
forLoopTrans :: Stm -> StmSA
forLoopTrans s = let (_,_,s') = tsa s (initV $ vars s)
                 in s'
