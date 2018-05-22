{-# LANGUAGE TemplateHaskell #-}
module Language.Options where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Data.DeriveTH

type File = String

data Opt =
    Ofile File
  | OsaHavoc  -- default
  | OsaFor
  | Obmc Int
  | Oassume   -- default
  | Oassert   
  | Odebug
  | OoutSA File
  | OoutVC File
  | OoutVCTex File
  | OoutWhy3 File
  | Owhy3
  | OVCGen String
  | OVOp String
  | Oversion
  | Ohelp
  deriving (Eq,Ord,Show)

$(derive makeIs ''Opt)

options :: [OptDescr Opt]
options =
     [ Option ['v','?'] ["version"] (NoArg Oversion) 
         "show version number"
     , Option ['h'] ["help"] (NoArg Ohelp)
         "show help"
     , Option [] ["sa-for"] (NoArg OsaFor)
       "FOR translation"
     , Option [] ["sa-havoc"] (NoArg OsaHavoc)
       "HAVOC translation (default)"
     , Option [] ["bmc"] (ReqArg (\s -> Obmc (read s)) "k")
       "unwind loops (k = 1 and unwinding assumption by default)"
     , Option [] ["assume"] (NoArg Oassume)
       "use unwinding assumption (default)"
     , Option [] ["assert"] (NoArg Oassert)
       "use unwinding assertion"
     , Option [] ["debug"] (NoArg Odebug)
       "debug intermediate representation"
     , Option [] ["out-sa"] (ReqArg (\s -> OoutSA s) "FILE")
       "output SA program"
     , Option [] ["out-vc"] (ReqArg (\s -> OoutVC s) "FILE")
       "output VC"
     , Option [] ["out-vc-tex"] (ReqArg (\s -> OoutVCTex s) "FILE")
       "output tex format"
     , Option [] ["out-why3"] (ReqArg (\s -> OoutWhy3 s) "FILE")
       "output why3 format"
     , Option [] ["why3"] (NoArg Owhy3)
       "call why3 with VCs"
     , Option [] ["vcgen"] (ReqArg (\s -> OVCGen s) "VCGen")
       "select VCGen (default: lin)"
     , Option [] ["vcgenop"] (ReqArg (\s -> OVOp s) "VCGenOp")
       "select option for VCGen (default: pa; possible: p,pa,g,ga)"
     ]

uHeader :: String
uHeader = "Usage: \n\n while-vcgen [OPTION...] file_name\n"

uInfo :: String
uInfo = usageInfo uHeader options

optionsParser :: [String] -> IO [Opt]
optionsParser a = 
  do case getOpt Permute options a of
       (o, [],[])  -> return o
       (o, (f:_),[])  -> return $ (Ofile f):o
       (_,_,e) -> ioError (userError (concat e ++ uInfo))
