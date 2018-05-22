module Main where

import System.Environment
import System.Directory
import Text.PrettyPrint
import Control.Monad.State
import System.Exit
import System.Process
import Data.List

import Language.Why3.PP

import Language.Options
import Language.While.Types
import Language.While.Parser
import Language.While.PrettyPrinter
import Language.While.Utils.Utils
import Language.WhileSA.Types
import Language.WhileSA.PrettyPrinter
import Language.Translations.TranslationFor
import Language.Translations.TranslationHavoc
import Language.VCGens.Base
import Language.VCGens.FrontEnd
import Language.Logic.Types
import Language.Logic.Why3Encoder
import Language.Logic.TexPrinter
import Language.LoopTreatement.LoopUnroll

showVersion :: Bool -> IO ()
showVersion False = return ()
showVersion _     = (putStrLn "while-lang-vcgen 0.2")
                    >> exitWith ExitSuccess

showHelp ::  Bool -> IO ()
showHelp False = return ()
showHelp _     = (putStrLn uInfo)
                 >> exitWith ExitSuccess

getProg :: Maybe File -> IO Stm
getProg Nothing =
  do parseOut <- loadStdin
     case parseOut of
       (Left e) -> putStrLn e >> exitFailure
       (Right stm) -> return stm
getProg (Just f) =
  do parseOut <- loadFile f
     case parseOut of
       (Left e) -> putStrLn e >> exitFailure
       (Right stm) -> return stm

loopUnroll :: Maybe Int -> Bool -> Stm -> Stm
loopUnroll Nothing _ s = s
loopUnroll (Just k) True s = loop_unroll AssertAnn k s
loopUnroll (Just k) False s = loop_unroll AssumeAnn k s

saTranslation :: Bool -> Stm -> StmSA
saTranslation True s = forLoopTrans s
saTranslation False s = havocTrans s

getVCGen :: Maybe String -> IO VCGen
getVCGen Nothing = return LIN
getVCGen (Just "sp" ) = return SP
getVCGen (Just "cnf") = return CNF
getVCGen (Just "lin") = return LIN
getVCGen (Just _    ) = ioError (userError "invalid vcgen")

getVCGenOp :: Maybe String -> IO VOp
getVCGenOp Nothing     = return VCPA
getVCGenOp (Just "p" ) = return VCP
getVCGenOp (Just "pa") = return VCPA
getVCGenOp (Just "g" ) = return VCG
getVCGenOp (Just "ga") = return VCGA
getVCGenOp (Just _   ) = ioError (userError "invalid vcgen option")

stdout :: (Pretty a) => Bool -> String -> a -> IO ()
stdout False _ _ = return ()
stdout _     s p = let s' = "\n** " ++ s ++ " **"
                   in putStrLn s' >> (putStrLn.render.pretty $ p)

fileOut :: (Pretty a) => Maybe File -> a -> IO ()
fileOut Nothing _  = return ()
fileOut (Just f) x = (writeFile f).render.pretty $ x


callWhy3 :: Bool -> [LExpr] -> IO ()
callWhy3 False _ = return ()
callWhy3 _ vcs   =
  do let dir = "why3-temp"
         createDirectoryIfMissing False dir
         let file = dir ++ "/vcs.why"
         fileOut (Just file) (ppTh $ logic2why3theory vcs)
         putStrLn $ "Wrote in " ++ file
         readProcess "why3" ["ide",file] [] >>= putStrLn
         removeDirectoryRecursive dir

main :: IO ()
main = 
  do args <- getArgs
     opts <- optionsParser args
     putStrLn (show opts)
     showHelp (elem Ohelp opts)
     showVersion (elem Oversion opts)
     stmIn <- getProg $ oFile opts
     stdout (elem Odebug opts) "Original Program" stmIn
     let stmBmc = loopUnroll (oBmc opts) (elem Oassert opts) stmIn
     stdout (elem Odebug opts) "Bounded Program" stmBmc
     let stmSA = saTranslation (elem OsaFor opts) stmBmc
     stdout (elem Odebug opts) "SA Program" stmSA
     fileOut (oOutSA opts) stmSA
     vcgen <- getVCGen $ oVCGen opts
     vcgenop <- getVCGenOp $ oVOp opts
     let vcs = generate stmSA vcgen vcgenop
     stdout (elem Odebug opts) "VCs" vcs
     fileOut (oOutVC opts) vcs
     fileOut (oOutVCTex opts) (toTexL vcs)
     fileOut (oOutWhy3 opts) (ppTh $ logic2why3theory vcs)
     callWhy3 (elem Owhy3 opts) vcs
  where
    oFile o     = find isOfile o     >>= (\(Ofile f) -> Just f)
    oBmc o      = find isObmc o      >>= (\(Obmc b) -> Just b)
    oOutSA o    = find isOoutSA o    >>= (\(OoutSA b) -> Just b)
    oOutVC o    = find isOoutVC o    >>= (\(OoutVC b) -> Just b)
    oOutVCTex o = find isOoutVCTex o >>= (\(OoutVCTex b) -> Just b)
    oOutWhy3 o  = find isOoutWhy3 o  >>= (\(OoutWhy3 b) -> Just b)
    oVCGen o    = find isOVCGen o    >>= (\(OVCGen g) -> Just g)
    oVOp o      = find isOVOp o      >>= (\(OVOp op) -> Just op)

