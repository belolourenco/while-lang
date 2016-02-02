module Main where

import System.Environment
import Text.PrettyPrint
import Control.Monad.State

import Language.While.Parser
import Language.While.PrettyPrinter
import Language.While.Utils.Utils
import Language.WhileSA.Types
import Language.WhileSA.PrettyPrinter
import Language.Translations.TranslationFor
import Language.Translations.TranslationHavoc
import Language.VCGens.FrontEnd
import Language.Logic.Types
import Language.LoopTreatement.LoopUnroll

parse_while_lang :: IO ()
parse_while_lang = do 
    r <- getArgs >>= loadFile.head
    case r of
        (Left error) -> putStrLn error
        (Right smt)  -> putStrLn.render.pretty $ smt

translation_to_SAFor :: IO ()
translation_to_SAFor = do 
    r <- getArgs >>= loadFile.head
    case r of
        (Left error) -> putStrLn error
        (Right smt)  -> putStrLn.render.pretty $ forLoopTrans smt

translation_to_SAHavoc :: IO ()
translation_to_SAHavoc = do 
    r <- getArgs >>= loadFile.head
    case r of
        (Left error) -> putStrLn error
        (Right smt)  -> putStrLn.render.pretty $ havocTrans smt

vc :: String -> VCGen
vc "psp" = PSP
vc "pspplus" = PSPPlus
vc "gsp" = GSP
vc "gspplus" = GSPPlus
vc "pcnf" = PCNF
vc "pcnfplus" = PCNFPlus
vc "gcnf" = GCNF
vc "gcnfplus" = GCNFPlus
vc "plin" = PLin
vc "plinplus" = PLinPlus
vc "glin" = GLin
vc "glinplus" = GLinPlus

ann :: String -> UnwindAnnotation
ann "assume" = AssumeAnn
ann "assert" = AssertAnn

pretty_list_vcs :: SetExpr -> Doc
pretty_list_vcs = vcat.(punctuate (text "\n - ")).(map pretty)

vcgen :: IO ()
vcgen = do 
    (file:a:_) <- getArgs
    content <- loadFile file
    case content of
        (Left error) -> putStrLn error
        (Right stm) -> putStrLn.render.pretty_list_vcs $ vcs (havocTrans stm) (vc a)

-- This is just a temporary solution. See omnigraffle diagram to make a general solution.
vcgen_iter :: IO ()
vcgen_iter = do
    (file:_) <- getArgs
    content <- loadFile file
    case content of
       (Left error) -> putStrLn error
       (Right stm) -> putStrLn.render.pretty_list_vcs.vcs_iter $ forLoopTrans stm

unwind :: IO()
unwind = do
    (file:annotation:bound:_) <- getArgs
    content <- loadFile file
    case content of
        (Left error) -> putStrLn error
        (Right stm) -> putStrLn.render.pretty $ loop_unroll (ann annotation) (read bound) stm