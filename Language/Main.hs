module Main where

import System.Environment
import Text.PrettyPrint
import Control.Monad.State
import System.Exit
import  System.Process

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
import Language.VCGens.FrontEnd
import Language.Logic.Types
import Language.Logic.Why3Encoder
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

vcgen :: IO ()
vcgen = do 
  (file:a:_) <- getArgs
  content <- loadFile file
  case content of
    (Left error) -> putStrLn error
    (Right stm) -> pretty_list_vcs $ vcs (havocTrans stm) (vc a)

-- This is just a temporary solution. See omnigraffle diagram to make a general solution.
vcgen_iter :: IO ()
vcgen_iter = do
  (file:_) <- getArgs
  content <- loadFile file
  case content of
    (Left error) -> putStrLn error
    (Right stm) -> pretty_list_vcs.vcs_iter $ forLoopTrans stm

unwind :: IO()
unwind = do
  (file:annotation:bound:_) <- getArgs
  content <- loadFile file
  case content of
    (Left error) -> putStrLn error
    (Right stm) -> putStrLn.render.pretty $ loop_unroll (ann annotation) (read bound) stm


pretty_list_vcs :: SetExpr -> IO ()
pretty_list_vcs s = putStrLn.render.vcat $ aux 0 $ map pretty s
  where
    aux :: Int -> [Doc] -> [Doc]
    aux x []    = []
    aux x (h:t) = (text "\n" <+> int x <+> text "-" <+> h):(aux (x+1) t)

output :: Bool -> SetExpr -> String -> IO ()
output True s  f = pretty_list_vcs s
output False s f = (writeFile why3file $ show $ ppTh (setExpr2why3theory s))
                    >> readProcess "why3" ["ide",why3file] [] >>= putStrLn
  where why3file = (takeWhile (/='.') f) ++ ".why"

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
    translation stm o@(Opts f v (Just a) (Just b) True _ _ _ pp)
      = let stm_unnwound   = loop_unroll a b stm
            stm_translated = havocTrans stm_unnwound
            vcs_result     = vcs stm_translated v
        in (output pp vcs_result f) >> exitSuccess
    translation stm o@(Opts _ v _ _ True _ _ _ pp)
      = putStrLn "Make sure you selected a bound and an unwinding annotation"
          >> exitFailure
    translation stm o@(Opts f v Nothing _ _ True _ _ pp) 
      = let stm_translated = havocTrans stm
            vcs_result     = vcs stm_translated v
        in (output pp vcs_result f) >> exitSuccess
    translation stm o@(Opts f v Nothing _ _ _ True _ pp) 
      = let stm_translated = forLoopTrans stm
            vcs_result     = vcs_iter stm_translated
        in (output pp vcs_result f) >> exitSuccess
    translation stm o@(Opts f v Nothing _ _ _ _ True pp)
      = putStrLn "NOT IMPLEMENTED YET!!!!\n" >> exitFailure