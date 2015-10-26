module Language.Translation.Translation where

import Data.List
import Control.Monad.State
import Data.Maybe

import Language.While.Utils.Utils
import Language.While.Parser
import Language.While.Types
import Language.WhileSA.Types

type VersionList = [VarnameSA]

-- | Increments a variable Version
next :: Version -> Version
next []    = [1]
next (h:t) = (h+1):t

-- | Appends a new element at Version head
new :: Version -> Version
new [] = [1]
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
init :: [Varname] -> State VersionList ()
init x = put $ map (\x -> (x,[0])) x


-- | Increments the version of the given variable. If the variable
-- is not in the list, it creates a new version for it, initializing it with 1.
-- next :: [Varname] -> Varname -> [Varname]
-- next [] x = 


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