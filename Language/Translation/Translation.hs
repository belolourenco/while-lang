module Language.Translation.Translation where

import Data.List
import Control.Monad.State
import Data.Maybe
import Text.PrettyPrint

import Language.While.Utils.Utils
import Language.While.Parser
import Language.While.Types
import Language.While.PrettyPrinter
import Language.WhileSA.Types
import Language.WhileSA.PrettyPrinter

type VersionList = [VarnameSA]

-- * Auxiliary functions

-- | Increments a variable Version
next :: Version -> Version
next []    = [1]
next (h:t) = (h+1):t

-- | Appends a new element at Version head
new :: Version -> Version
new t  = 1:t

-- | It removes the first element of the Version list and increments the second
jump :: Version -> Version
jump []      = error "jump with empty list" -- it should never be reachable
jump (_:[])  = error "jump with only one element" -- it should never be reachable
jump (_:j:t) = (j+1):t

-- | Merges two Version lists, choosing for each variable the highest Version
sup :: VersionList -> VersionList -> VersionList
sup  v1 v2 = map aux $ nub $ (map fst v1) ++ (map fst v2)
  where aux x = (x, greater (lookup x v1) (lookup x v2))

        greater :: Maybe Version -> Maybe Version -> Version
        greater Nothing v           = fromJust v
        greater v Nothing           = fromJust v
        greater (Just v1) (Just v2) = if (head v1) > (head v2) then v1 else v2 

-- | It creates a Rnm, whose domain contains the variables from the first Version
-- list that are smaller than the version in the second list
merge :: VersionList -> VersionList -> Rnm
merge v1 v2 = map aux2 $ filter aux v1
  where
    aux :: VarnameSA -> Bool
    aux x = (snd x) < (fromJust $ lookup (fst x) v2)

    aux2 :: VarnameSA -> (VarnameSA, VarnameSA)
    aux2 x = ((fst x, fromJust $ lookup (fst x) v2), x)

-- | Returns the domain of a Rnm
dom :: Rnm -> [VarnameSA]
dom = map fst

-- | It returns a Rnm whose left side is the jump of the right side
upd :: [VarnameSA] -> Rnm
upd = map (\(x,y) -> ((x,jump y), (x,y)))

-- | Initializes a list of variables with version 0.
initV :: [Varname] -> VersionList
initV x = map (\x -> (x,[0])) x

---------------------------------------
-- * Auxiliary functions for the State Monad

-- | Increments the version of the given variable. If the variable
-- is not in the list, it creates a new version for it, initializing it with 1.
nextVarAux :: Varname -> VersionList -> VersionList
nextVarAux x []    = [(x,[1])]
nextVarAux x ((n,v):t) | n == x = let (vi:vs) = v
                                     in (n,(vi+1):vs):t
                          | n /= x = (n,v):(nextVarAux x t)

-- | Increments the version of the given variable. If the variable
-- is not in the list, it creates a new version for it, initializing it with 1.
nextVar :: Varname -> State VersionList ()
nextVar n = get >>= put.(nextVarAux n)


-- | Returns the current version of a Variable
getVarVerAux :: Varname -> VersionList -> Version
getVarVerAux n vs = case lookup n vs of
                      Nothing -> [0]
                      Just v  -> v

-- | Returns the current version of a Variable
getVarVer :: Varname -> State VersionList Version
getVarVer n = get >>= \x -> return $ getVarVerAux n x

-- | Returns the an SA variable with the current version of a Variable
getVar :: Varname -> State VersionList VarnameSA
getVar n = getVarVer n >>= \x -> return $ (n,x)

-- | Appends a new index in the SA variable
newVarAux :: Varname -> VersionList -> VersionList
newVarAux x []    = [(x,[1])]
newVarAux x ((n,v):t) | n == x = (n,new v):t
                      | otherwise = (n,v):(newVarAux x t)

-- | Appends a new index in the SA variable
newVar :: Varname -> State VersionList ()
newVar n = get >>= put.(newVarAux n)

-- | Replaces the version of an SA variable with the given version
replacesVarAux :: Varname -> Version -> VersionList -> VersionList
replacesVarAux x nv []          = [(x,nv)]
replacesVarAux x nv ((n,v):t) | x == n = (n,nv):t
                              | otherwise = (n,v):(replacesVarAux x nv t)

-- | Replaces the version of an SA variable with the given version
replacesVar :: Varname -> Version -> State VersionList ()
replacesVar x nv = get >>= put.(replacesVarAux x nv)

-- * Translation

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
tsaBexp (Bneg b)     = tsaBexp b >>= \x -> return $ BnegSA x
tsaBexp (Band b1 b2) = tsaBexp b1 >>= \x -> tsaBexp b2
                                  >>= \y -> return $ BandSA x y


tsa :: Stm -> State VersionList StmSA
tsa (Sass n e)    = tsaAexp e >>= \e' -> nextVar n 
                           >>  getVar n
                           >>= \n' -> return $ SassSA n' e'
tsa Sskip         = return SskipSA
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
--tsa (Stry Stm Stm

transFile :: FilePath -> IO ()
transFile path = do
  p <- loadFile path
  case p of
    Left e -> putStrLn.show $ e
    Right stm -> putStrLn.render.pretty $ evalState (tsa stm) (initV $ vars stm)

---------------------------------------
-- * Tests
testnext1 = next []
testnext2 = next [1]
testnext3 = next [1,2]
testnext4 = next [3,1,5]

testnew1 = new []
testnew2 = new [1]
testnew3 = new [1,2]
testnew4 = new [3,1,5]

testjump1 = jump []
testjump2 = jump [1]
testjump3 = jump [1,2]
testjump4 = jump [3,1,5]

testsup1 = sup [] []
testsup2 = sup [("x",[1])] []
testsup3 = sup [] [("x",[1])]
testsup4 = sup [("x",[1,2,3]),("y",[2]),("z", [5])] [("x",[1]),("y",[3]),("z",[4])]
testsup5 = sup [("x",[1]),("z", [5])] [("x",[1]),("z",[4])]
testsup6 = sup [("z", [5])] [("z",[4])]
testsup7 = sup [("x",[1]),("y",[3]),("z", [5])] [("x",[1]),("y",[3]),("z",[4])]

testmerge1 = merge [] []
testmerge2 = merge [("x",[1])] []
testmerge3 = merge [] [("x",[1])]
testmerge4 = merge [("x",[1,2,3]),("y",[2]),("z", [5])] [("x",[1]),("y",[3]),("z",[4])]
testmerge5 = merge [("x",[1]),("z", [5])] [("x",[1]),("z",[4])]
testmerge6 = merge [("x",[1]),("z", [5])] [("x",[2]),("z",[4])]
testmerge7 = merge [("x",[1]),("y",[3]),("z", [5])] [("x",[1]),("y",[3]),("z",[4])]
testmerge8 = merge [("x",[1]),("y",[3]),("z", [5])] [("x",[2]),("y",[3]),("z",[10])]

testupd1 = upd []
testupd2 = upd [("x",[1])]
testupd3 = upd [("x",[1,2,3]),("y",[4,2]),("z", [2,5,9,0])]

testNextVarVer1 = nextVarAux "x" []
testNextVarVer2 = nextVarAux "x" [("x",[1])] 
testNextVarVer3 = nextVarAux "y" [("x",[1])] 
testNextVarVer4 = nextVarAux "y" [("x",[1,2,3]),("y",[2]),("z", [5])]  
testNextVarVer5 = nextVarAux "x" [("x",[1]),("z", [5])] 
testNextVarVer6 = nextVarAux "z" [("x",[1]),("z", [5])]
testNextVarVer7 = nextVarAux "x" [("x",[1]),("y",[3]),("z", [5])]
testNextVarVer8 = nextVarAux "z" [("x",[1]),("y",[3]),("z", [5])]
testNextVarVer9 = nextVarAux "x" [("x",[1,2,3]),("y",[2]),("z", [5])] 