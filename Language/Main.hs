module Main where

import System.Environment
import Text.PrettyPrint
import Control.Monad.State
import System.Exit

import Language.Options
import Language.While.Types
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

main :: IO a
main = 
  do args <- getArgs
     opts <- optionsParser args
     case opts of
       (Left e)  -> putStrLn e >> exitFailure
       (Right o) -> do fContent <- loadFile $ optFile o
                       case fContent of
                            (Left e) -> putStrLn e >> exitFailure
                            (Right stm) -> translation stm o
  where
    translation :: Stm -> Opts -> IO a
    translation stm o@(Opts _ v (Just a) (Just b) True _ _ _)
      = let stm_unnwound   = loop_unroll a b stm
            stm_translated = havocTrans stm_unnwound
            vcs_result     = vcs stm_translated v
        in (putStrLn.render.pretty_list_vcs $ vcs_result) >> exitSuccess
    translation stm o@(Opts _ v _ _ True _ _ _)
      = putStrLn "Make sure you selected a bound and an unwinding annotation"
          >> exitFailure
    translation stm o@(Opts _ v Nothing _ _ True _ _) 
      = let stm_translated = havocTrans stm
            vcs_result     = vcs stm_translated v
        in (putStrLn.render.pretty_list_vcs $ vcs_result) >> exitSuccess
    translation stm o@(Opts _ v Nothing _ _ _ True _) 
      = let stm_translated = forLoopTrans stm
            vcs_result     = vcs_iter stm_translated
        in (putStrLn.render.pretty_list_vcs $ vcs_result) >> exitSuccess
    translation stm o@(Opts _ v Nothing _ _ _ _ True)
      = putStrLn "NOT IMPLEMENTED YET!!!!\n" >> exitFailure