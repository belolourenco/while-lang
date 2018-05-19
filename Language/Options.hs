module Language.Options where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Opt =
    OinFile File
  | OsaHavoc  -- default
  | OsaFor
  | Obmc Int
  | Oassume   -- default
  | Oassert   
  | Odebug
  | OoutSA File
  | OoutVC File
  | Otex
  | OVCGen String
  | Oversion
  | Ohelp
  deriving (Eq,Show)

type File = String

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
     , Option [] ["out-sa"] (ReqArg (\s -> OoutSA s) "FILE")
       "output SA program"
     , Option [] ["out-vc"] (ReqArg (\s -> OoutVC s) "FILE")
       "output VC"
     , Option [] ["tex"] (NoArg Otex)
       "output tex format"
     , Option [] ["vcgen"] (ReqArg (\s -> OVCGen s) "VCGen")
       "select VCGen (default: linpa)"
     ]

showVersion :: IO a
showVersion = do putStrLn "while-lang-vcgen 0.2"
                 exitWith ExitSuccess

showHelp ::  IO a
showHelp = (putStrLn $ usageInfo usage_header options)
           >>  exitWith ExitSuccess

usage_header = "Usage: \n\n while-vcgen [OPTION...] file_name\n"
  
optionsParser :: [String] -> IO [Opt]
optionsParser a = 
  do case getOpt Permute options a of
       (o, [],[])  -> return o
       (o, (f:_),[])  -> return $ (OinFile f):o
       (_,_,e) -> ioError (userError (concat e ++ usageInfo usage_header options))
