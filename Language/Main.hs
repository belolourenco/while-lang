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

toTex :: BexpSA -> Doc
toTex BtrueSA        = text "true"
toTex BfalseSA       = text "false"
toTex (BVariableSA n)= pretty n
toTex (BeqSA e1 e2)  = pretty e1 <> text " = " <> pretty e2
toTex (BleqSA e1 e2) = pretty e1 <> text " \\le " <> pretty e2
toTex (BlSA e1 e2)   = pretty e1 <> text " < " <> pretty e2
toTex (BgSA e1 e2)   = pretty e1 <> text " > " <> pretty e2
toTex (BgeqSA e1 e2) = pretty e1 <> text " \\ge " <> pretty e2
toTex (BnegSA e1)     = l <> text " \\neg " <> toTex e1 <> r
toTex (BandSA e1 e2)  = l <> toTex e1 <> text " \\andd " <> toTex e2 <> r
toTex (BorSA e1 e2)   = l <> toTex e1 <> text " \\orr " <> toTex e2 <> r
toTex (BimplSA e1 e2) = l <> toTex e1 <> text " \\to " <> toTex e2 <> r


pretty_list_vcs :: SetExpr -> IO ()
pretty_list_vcs s = putStrLn.render.vcat $ aux 0 $ map pretty s
  where
    aux :: Int -> [Doc] -> [Doc]
    aux x []    = []
    aux x (h:t) = (text "\n" <+> int x <+> text "-" <+> h):(aux (x+1) t)

pretty_tex_list_vcs :: SetExpr -> IO ()
pretty_tex_list_vcs s = putStrLn.render.vcat $ wrap $ map (\i -> text "\\item $" <> i <> text "$")
                                                    $ map toTex s
  where wrap l = text "\\begin{enumerate}" : l ++ [text "\\end{enumerate}"]


-- output :: PPFormat -> SetExpr -> String -> IO ()
-- output PPNormal s f = pretty_list_vcs s
-- output PPTex s f = pretty_tex_list_vcs s
-- output PPNone s f = (writeFile why3file $ show $ ppTh (setExpr2why3theory s))
--                     >> readProcess "why3" ["ide",why3file] [] >>= putStrLn
--   where why3file = (takeWhile (/='.') f) ++ ".why"

getProg :: Maybe Opt -> IO Stm
getProg Nothing =
  do parseOut <- loadStdin
     case parseOut of
       (Left e) -> putStrLn e >> exitFailure
       (Right stm) -> return stm
getProg (Just (OinFile f)) =
  do parseOut <- loadFile f
     case parseOut of
       (Left e) -> putStrLn e >> exitFailure
       (Right stm) -> return stm

loopUnroll :: Maybe Opt -> Bool -> Stm -> Stm
loopUnroll Nothing _ s = s
loopUnroll (Just (Obmc k)) True s = loop_unroll AssertAnn k s
loopUnroll (Just (Obmc k)) False s = loop_unroll AssumeAnn k s

saTranslation :: Bool -> Stm -> StmSA
saTranslation True s = forLoopTrans s
saTranslation False s = havocTrans s

getVCGen :: Maybe Opt -> IO VCGen
getVCGen Nothing = return LIN
getVCGen (Just (OVCGen "sp" )) = return SP
getVCGen (Just (OVCGen "cnf")) = return CNF
getVCGen (Just (OVCGen "lin")) = return LIN
getVCGen (Just (OVCGen _))     = ioError (userError "invalid vcgen")

getVCGenOp :: Maybe Opt -> IO VOp
getVCGenOp Nothing            = return VCPA
getVCGenOp (Just (OVOp "p"))  = return VCP
getVCGenOp (Just (OVOp "pa")) = return VCPA
getVCGenOp (Just (OVOp "g"))  = return VCG
getVCGenOp (Just (OVOp "ga")) = return VCGA
getVCGenOp (Just (OVOp _))    = ioError (userError "invalid vcgen option")

main :: IO ()
main = 
  do args <- getArgs
     opts <- optionsParser args
     putStrLn (show opts)
     if elem Ohelp opts then showHelp else return ()
     if elem Oversion opts then showVersion else return ()
     stmIn <- getProg (find isInFile opts)
     let stmBmc = loopUnroll (find doBmc opts) (elem Oassert opts) stmIn
     let stmSA = saTranslation (elem OsaFor opts) stmBmc
     vcgen <- getVCGen $ find selVCGen opts
     vcgenop <- getVCGenOp $ find selVCGenOp opts
     let vcs = generate stmSA vcgen vcgenop
     -- putStrLn.render.pretty $ vcs
     pretty_list_vcs vcs
  where
    isInFile (OinFile f) = True
    isInFile _ = False

    doBmc (Obmc _) = True
    doBmc _ = False

    selVCGen (OVCGen _) = True
    selVCGen _ = False

    selVCGenOp (OVOp _) = True
    selVCGenOp _ = False
