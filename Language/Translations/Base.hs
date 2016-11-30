module Language.Translations.Base where

import Data.List
import Data.Maybe
import Control.Monad.State

import Language.WhileSA.Types
import Language.While.Types
import Language.While.Utils.Utils

type Versions = [VarnameSA]

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
sup :: Maybe Versions -> Maybe Versions -> Maybe Versions
sup Nothing v2 = v2
sup v1 Nothing = v1
sup  (Just v1) (Just v2) =
  Just $ map aux $ nub $ (map fst v1) ++ (map fst v2)
  where aux x = (x, greater (lookup x v1) (lookup x v2))

        greater :: Maybe Version -> Maybe Version -> Version
        greater Nothing v           = fromJust v
        greater v Nothing           = fromJust v
        greater (Just v1) (Just v2) =
          if (head v1) > (head v2) then v1 else v2 

-- | It creates a Rnm, whose domain contains the variables from the first Version
-- list that are smaller than the version in the second list
merge :: Maybe Versions -> Maybe Versions -> Rnm
merge Nothing v2 = []
merge v1 Nothing = []
merge (Just v1) (Just v2) = map aux2 $ filter aux v1
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
initV :: [Varname] -> Versions
initV x = map (\x -> (x,[0])) x

---------------------------------------
-- * Auxiliary functions for the State Monad

-- | Increments the version of the given variable. If the variable
-- is not in the list, it creates a new version for it, initializing it with 1.
nextVar :: Varname -> Versions -> Versions
nextVar x []    = [(x,[1])]
nextVar x ((n,v):t) | n == x = let (vi:vs) = v
                               in (n,(vi+1):vs):t
                    | n /= x = (n,v):(nextVar x t)

-- | Increments the version of the given variable. If the variable
-- is not in the list, it creates a new version for it, initializing it with 1.
-- nextVar :: Varname -> State Versions ()
-- nextVar n = get >>= put.(nextVarAux n)


-- | Returns the current version of a Variable
getVarVer :: Varname -> Versions -> Version
getVarVer n vs = case lookup n vs of
                   Nothing -> [0]
                   Just v  -> v

getVar :: Varname -> Versions -> VarnameSA
getVar n vs = (n,(getVarVer n vs))

-- | Returns the current version of a Variable
-- getVarVer :: Varname -> State Versions Version
-- getVarVer n = get >>= \x -> return $ getVarVerAux n x

-- | Returns the an SA variable with the current version of a Variable
-- getVar :: Varname -> State Versions VarnameSA
-- getVar n = getVarVer n >>= \x -> return $ (n,x)

-- | Appends a new index in the SA variable
newVar :: Varname -> Versions -> Versions
newVar x []    = [(x,[1])]
newVar x ((n,v):t) | n == x = (n,new v):t
                   | otherwise = (n,v):(newVar x t)

-- | Appends a new index in the SA variable
-- newVar :: Varname -> State Versions ()
-- newVar n = get >>= put.(newVarAux n)

-- | Replaces the version of an SA variable with the given version
replacesVar :: Varname -> Version -> Versions -> Versions
replacesVar x nv []          = [(x,nv)]
replacesVar x nv ((n,v):t) | x == n = (n,nv):t
                           | otherwise = (n,v):(replacesVar x nv t)

-- | Replaces the version of an SA variable with the given version
-- replacesVar :: Varname -> Version -> State Versions ()
-- replacesVar x nv = get >>= put.(replacesVarAux x nv)

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

testsup1 = sup (Just []) (Just [])
testsup2 = sup (Just [("x",[1])]) (Just [])
testsup3 = sup (Just []) (Just [("x",[1])])
testsup4 = sup (Just [("x",[1,2,3]),("y",[2]),("z", [5])])
               (Just [("x",[1]),("y",[3]),("z",[4])])
testsup5 = sup (Just [("x",[1]),("z", [5])]) (Just [("x",[1]),("z",[4])])
testsup6 = sup (Just [("z", [5])]) (Just [("z",[4])])
testsup7 = sup (Just [("x",[1]),("y",[3]),("z", [5])])
               (Just [("x",[1]),("y",[3]),("z",[4])])

testmerge1 = merge (Just []) (Just [])
testmerge2 = merge (Just [("x",[1])]) (Just [])
testmerge3 = merge (Just []) (Just [("x",[1])])
testmerge4 = merge (Just [("x",[1,2,3]),("y",[2]),("z", [5])])
                   (Just [("x",[1]),("y",[3]),("z",[4])])
testmerge5 = merge (Just [("x",[1]),("z", [5])]) (Just [("x",[1]),("z",[4])])
testmerge6 = merge (Just [("x",[1]),("z", [5])]) (Just [("x",[2]),("z",[4])])
testmerge7 = merge (Just [("x",[1]),("y",[3]),("z", [5])])
                   (Just [("x",[1]),("y",[3]),("z",[4])])
testmerge8 = merge (Just [("x",[1]),("y",[3]),("z", [5])])
                   (Just [("x",[2]),("y",[3]),("z",[10])])

testupd1 = upd []
testupd2 = upd [("x",[1])]
testupd3 = upd [("x",[1,2,3]),("y",[4,2]),("z", [2,5,9,0])]

testNextVarVer1 = nextVar "x" []
testNextVarVer2 = nextVar "x" [("x",[1])] 
testNextVarVer3 = nextVar "y" [("x",[1])] 
testNextVarVer4 = nextVar "y" [("x",[1,2,3]),("y",[2]),("z", [5])]  
testNextVarVer5 = nextVar "x" [("x",[1]),("z", [5])] 
testNextVarVer6 = nextVar "z" [("x",[1]),("z", [5])]
testNextVarVer7 = nextVar "x" [("x",[1]),("y",[3]),("z", [5])]
testNextVarVer8 = nextVar "z" [("x",[1]),("y",[3]),("z", [5])]
testNextVarVer9 = nextVar "x" [("x",[1,2,3]),("y",[2]),("z", [5])] 
