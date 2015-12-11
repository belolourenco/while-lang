module Main where

import System.Environment
import Text.PrettyPrint

import Language.While.Parser
import Language.While.PrettyPrinter
import Language.WhileSA.PrettyPrinter
import Language.Translations.TranslationFor
import Language.Translations.TranslationHavoc
import Language.VCGens.FrontEnd

parse_while_lang :: IO ()
parse_while_lang = do 
    r <- getArgs >>= loadFile.head
    case r of
        (Left error) -> putStrLn error
        (Right smt)  -> putStrLn.render.pretty $ smt

translation_to_SAFor :: IO ()
translation_to_SAFor = do 
    r <- getArgs >>= transFileSAFor.head
    case r of
        (Left error) -> putStrLn error
        (Right smt)  -> putStrLn.render.pretty $ smt

translation_to_SAHavoc :: IO ()
translation_to_SAHavoc = do 
    r <- getArgs >>= transFileSALF.head
    case r of
        (Left error) -> putStrLn error
        (Right smt)  -> putStrLn.render.pretty $ smt

vc :: String -> VCGen
vc "psp" = PSP
vc "pspplus" = PSPPlus
vc "gsp" = GSP
vc "gspplus" = GSPPlus
vc "pcnf" = PCNF
vc "pcnfplus" = PCNFPlus
vc "gcnf" = GCNF
vc "gcnfplus" = GCNFPlus

vcgen :: IO ()
vcgen = do 
    (file:a:_) <- getArgs
    smt <- transFileSALF file
    case smt of
        (Left error) -> putStrLn error
        (Right smt') -> putStrLn.render.vcat.(punctuate (text "\n - ")).(map pretty) $ vcs smt' (vc a)