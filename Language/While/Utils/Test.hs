module Test where

import Text.PrettyPrint
import Language.While.Parser
import Language.While.PrettyPrinter

main :: FilePath -> IO ()
main path = loadFile path >>= aux
  where aux (Left s) = putStrLn s
        aux (Right stm) = putStrLn.render $ pretty stm