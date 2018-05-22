module Main where

import System.Environment
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

-- parse_while_lang :: IO ()
-- parse_while_lang = do 
--   r <- getArgs >>= loadFile.head
--   case r of
--     (Left error) -> putStrLn error
--     (Right smt)  -> putStrLn.render.pretty $ smt

-- translation_to_SAFor :: IO ()
-- translation_to_SAFor = do 
--   r <- getArgs >>= loadFile.head
--   case r of
--     (Left error) -> putStrLn error
--     (Right smt)  -> putStrLn.render.pretty $ forLoopTrans smt

-- translation_to_SAHavoc :: IO ()
-- translation_to_SAHavoc = do
--   version:file:_ <- getArgs
--   r <- loadFile file
--   case r of
--     (Left error) -> putStrLn error
--     (Right smt)  -> putStrLn.render.pretty $ havocTrans smt

-- vcgen :: IO ()
-- vcgen = do 
--   (file:a:_) <- getArgs
--   content <- loadFile file
--   case content of
--     (Left error) -> putStrLn error
--     (Right stm) -> pretty_list_vcs $ vcs (havocTrans stm) (vc a)
--     where vc :: String -> VCGen
--           vc "psp" = PSP
--           vc "pspplus" = PSPPlus
--           vc "gsp" = GSP
--           vc "gspplus" = GSPPlus
--           vc "pcnf" = PCNF
--           vc "pcnfplus" = PCNFPlus
--           vc "gcnf" = GCNF
--           vc "gcnfplus" = GCNFPlus
--           vc "plin" = PLin
--           vc "plinplus" = PLinPlus
--           vc "glin" = GLin
--           vc "glinplus" = GLinPlus

-- -- This is just a temporary solution. See omnigraffle diagram to make a general solution.
-- vcgen_iter :: IO ()
-- vcgen_iter = do
--   (file:_) <- getArgs
--   content <- loadFile file
--   case content of
--     (Left error) -> putStrLn error
--     (Right stm) -> pretty_list_vcs.vcs_iter $ forLoopTrans stm

-- unwind :: IO()
-- unwind = do
--   (file:annotation:bound:_) <- getArgs
--   content <- loadFile file
--   case content of
--     (Left error) -> putStrLn error
--     (Right stm) -> putStrLn.render.pretty $ loop_unroll (ann annotation) (read bound) stm
--   where
--     ann :: String -> UnwindAnnotation
--     ann "assume" = AssumeAnn
--     ann "assert" = AssertAnn

-- output :: PPFormat -> SetExpr -> String -> IO ()
-- output PPNormal s f = pretty_list_vcs s
-- output PPTex s f = pretty_tex_list_vcs s
-- output PPNone s f = (writeFile why3file $ show $ ppTh (setExpr2why3theory s))
--                     >> readProcess "why3" ["ide",why3file] [] >>= putStrLn
--   where why3file = (takeWhile (/='.') f) ++ ".why"


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

texPrint :: Maybe File -> [LExpr] -> IO ()
texPrint f vcs = fileOut f (toTexL vcs)

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
     let stmSA = saTranslation (elem OsaFor opts) stmBmc
     fileOut (oOutSA opts) stmSA
     stdout (elem Odebug opts) "SA Program" stmSA
     vcgen <- getVCGen $ oVCGen opts
     vcgenop <- getVCGenOp $ oVOp opts
     let vcs = generate stmSA vcgen vcgenop
     fileOut (oOutVC opts) vcs
     texPrint (oOutVCTex opts) vcs
     stdout (elem Odebug opts) "VCs" vcs
  where
    oFile o     = find isOfile o     >>= (\(Ofile f) -> Just f)
    oBmc o      = find isObmc o      >>= (\(Obmc b) -> Just b)
    oOutSA o    = find isOoutSA o    >>= (\(OoutSA b) -> Just b)
    oOutVC o    = find isOoutVC o    >>= (\(OoutVC b) -> Just b)
    oOutVCTex o = find isOoutVCTex o >>= (\(OoutVCTex b) -> Just b)
    oVCGen o    = find isOVCGen o    >>= (\(OVCGen g) -> Just g)
    oVOp o      = find isOVOp o      >>= (\(OVOp op) -> Just op)

