module Language.Options where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Language.VCGens.FrontEnd
import Language.LoopTreatement.LoopUnroll

data Opts = Opts
  { optFile    :: String
  , optVCGen   :: VCGen
  , optLoopAnn :: Maybe UnwindAnnotation
  , optBound   :: Maybe Integer
  , optLoopUnw :: Bool
  , optHavoc   :: Bool
  , optForDV   :: Bool
  , optForBV   :: Bool
  } deriving Show

optsInit = Opts "" PSP Nothing Nothing False False False False

options :: [OptDescr (Opts -> IO Opts)]
options =
     [ Option ['v','?'] ["version"]
         (NoArg showVersion) 
         "show version number"
     , Option ['h'] ["help"]
         (NoArg showHelp)
         "show help"
     , Option [] ["unwind_loops"]
         (NoArg (\o -> return $ o {optLoopUnw = True}))
         "unwind loops (requires 'unwind_bound' and 'unwind_annotation')"
     , Option [] ["havoc_trans"]
         (NoArg (\o -> return $ o {optHavoc = True}))
         "Havoc translation"
     , Option [] ["for_loops_dv"]
         (NoArg (\o -> return $ o {optForDV = True}))
         "FOR loops translation with Deductive Verification"
     , Option [] ["for_loops_dv"]
         (NoArg (\o -> return $ o {optForBV = True}))
         "FOR loops translation with Bounded Verification"
     , Option ['b'] ["unwind_bound"]
         (ReqArg (\a o -> return $ o {optBound = Just (read a)}) "N")
         "unwinding bound"
     , Option ['a'] ["unwind_annotation"]
         (ReqArg (\a o -> return $ o {optLoopAnn = Just $ ann a}) "{assume|assert}")
         "unwinding annotation"
     , Option ['g'] ["vcgen"]
         (ReqArg (\a o -> return $ o {optVCGen = vc a}) "vcgen")
         "VCGen algorithm vcgen={psp,pspplus,gps,gpsplus,pcnf,pcnfplus,gcnf,gcnfplus,plin,plinplus,glin,glinplus}"
     ]

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

showVersion :: t -> IO b
showVersion _ = do putStrLn "while_lang_vcge 0.1"
                   exitWith ExitSuccess

showHelp :: t -> IO b
showHelp _ = (putStrLn $ usageInfo usage_header options)
             >>  exitWith ExitSuccess
  where
    usage_header = "Usage: \n\n while-vcgen [OPTION...] file_name\n"

optionsParser :: [String] -> IO (Either String Opts)
optionsParser a = 
  do case getOpt Permute options a of
       (o, [], [])    -> do opts <- foldl (>>=) (return $ optsInit) o 
                            return.Left $ "File name missing"
       (o, (f:_),[])  -> do opts <- foldl (>>=) (return $ optsInit {optFile = f}) o 
                            return.Right $ opts