module Language.Translations.TranslationFor where

import Control.Monad.State

import Language.While.Utils.Utils
import Language.While.Parser
import Language.While.Types
import Language.WhileSA.Types
import Language.Translations.Base

-- * Translation for SA-For language

tsaAexp :: Aexp -> State VersionList AexpSA
tsaAexp (Numeral i) = return $ NumeralSA i
tsaAexp (Variable n) = getVar n >>= \x -> return $ VariableSA x
tsaAexp (Aadd e1 e2) = tsaAexp e1 >>= \x -> tsaAexp e2 
                                  >>= \y -> return $ AaddSA x y
tsaAexp (Asub e1 e2) = tsaAexp e1 >>= \x -> tsaAexp e2 
                                  >>= \y -> return $ AsubSA x y
tsaAexp (Amul e1 e2) = tsaAexp e1 >>= \x -> tsaAexp e2 
                                  >>= \y -> return $ AmulSA x y
tsaAexp (Adiv e1 e2) = tsaAexp e1 >>= \x -> tsaAexp e2 
                                  >>= \y -> return $ AdivSA x y

tsaBexp :: Bexp -> State VersionList BexpSA
tsaBexp Btrue        = return BtrueSA
tsaBexp Bfalse       = return BfalseSA
tsaBexp (Beq e1 e2)  = tsaAexp e1 >>= \x -> tsaAexp e2 
                                  >>= \y -> return $ BeqSA x y
tsaBexp (Bleq e1 e2) = tsaAexp e1 >>= \x -> tsaAexp e2 
                                  >>= \y -> return $ BleqSA x y
tsaBexp (Bl e1 e2)   = tsaAexp e1 >>= \x -> tsaAexp e2 
                                  >>= \y -> return $ BlSA x y
tsaBexp (Bg e1 e2)   = tsaAexp e1 >>= \x -> tsaAexp e2 
                                  >>= \y -> return $ BgSA x y
tsaBexp (Bgeq e1 e2) = tsaAexp e1 >>= \x -> tsaAexp e2 
                                  >>= \y -> return $ BgeqSA x y
tsaBexp (Bneg b)     = tsaBexp b >>= \x -> return $ BnegSA x
tsaBexp (Band b1 b2) = tsaBexp b1 >>= \x -> tsaBexp b2
                                  >>= \y -> return $ BandSA x y


tsa :: Stm -> State VersionList StmSA
tsa (Sass n e)    = tsaAexp e >>= \e' -> nextVar n 
                           >>  getVar n
                           >>= \n' -> return $ SassSA n' e'
tsa Sskip         = return SskipSA
tsa (Sassume b)   = tsaBexp b >>= return.SassumeSA
tsa (Sassert b)   = tsaBexp b >>= return.SassertSA
tsa (Scomp s1 s2) = tsa s1 >>= \s1' -> tsa s2
                           >>= \s2' -> return $ ScompSA s1' s2'
tsa (Sif b s1 s2) = do
  v <- get
  b'  <- tsaBexp b
  s1' <- tsa s1
  v' <- get
  put v
  s2' <- tsa s2
  v'' <- get
  put $ sup v' v''
  return $ SifSA b' (ScompSA s1' (rnmToAssign $ merge v' v'')) 
                    (ScompSA s2' (rnmToAssign $ merge v'' v'))
tsa (Swhile b s) = do
  let asgn_c = asgn s
  v <- get
  i <- create_i asgn_c
  v' <- upd_v' asgn_c
  b' <- tsaBexp b
  s' <- tsa s
  u <- create_u asgn_c v
  let dom_u = map fst u
  mapM_ (\x -> replacesVar (fst x) (jump $ snd x)) dom_u
  return $ ScompSA  (SforSA i b' u s')
                    (rnmToAssign $ upd dom_u)
  where
    create_i :: [Varname] -> State VersionList Rnm
    create_i []     = return []
    create_i (n:ns) = getVarVer n >>= \v -> create_i ns 
                                  >>= \ns' -> return $ ((n,new v), (n, v)):ns'

    upd_v' :: [Varname] -> State VersionList VersionList
    upd_v' []     = get >>= return
    upd_v' (n:ns) = newVar n >> upd_v' ns

    create_u :: [Varname] -> VersionList -> State VersionList Rnm
    create_u []     vl = return []
    create_u (n:ns) vl = getVarVer n >>= \v' -> create_u ns vl
                                     >>= \ns' -> return $ ((n, new $ getVarVerAux n vl),(n,v')):ns'
tsa (SwhileInv b inv s) = do
  let asgn_c = asgn s
  v <- get
  i <- create_i asgn_c
  v' <- upd_v' asgn_c
  b' <- tsaBexp b
  inv' <- tsaBexp inv
  s' <- tsa s
  u <- create_u asgn_c v
  let dom_u = map fst u
  mapM_ (\x -> replacesVar (fst x) (jump $ snd x)) dom_u
  return $ ScompSA  (SforInvSA i b' u inv' s')
                    (rnmToAssign $ upd dom_u)
  where
    create_i :: [Varname] -> State VersionList Rnm
    create_i []     = return []
    create_i (n:ns) = getVarVer n >>= \v -> create_i ns 
                                  >>= \ns' -> return $ ((n,new v), (n, v)):ns'

    upd_v' :: [Varname] -> State VersionList VersionList
    upd_v' []     = get >>= return
    upd_v' (n:ns) = newVar n >> upd_v' ns

    create_u :: [Varname] -> VersionList -> State VersionList Rnm
    create_u []     vl = return []
    create_u (n:ns) vl = getVarVer n >>= \v' -> create_u ns vl
                                     >>= \ns' -> return $ ((n, new $ getVarVerAux n vl),(n,v')):ns'
--tsa (Stry Stm Stm

transFileSAFor :: FilePath -> IO (Either String StmSA)
transFileSAFor path = do
  p <- loadFile path
  case p of
    Left e -> return.Left $ e
    Right stm -> return.Right $ evalState (tsa stm) (initV $ vars stm)