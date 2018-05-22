module Language.Logic.TexPrinter where

import Text.PrettyPrint

import Language.Logic.Types
import Language.WhileSA.Types
import Language.While.PrettyPrinter
import Language.WhileSA.PrettyPrinter

toTex :: BexpSA -> Doc
toTex BtrueSA        = text "\true"
toTex BfalseSA       = text "\false"
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

toTexL :: [LExpr] -> Doc
toTexL s = vcat $ wrap $ map (\i -> text "\\item $" <> i <> text "$")
           $ map toTex s
  where wrap l = text "\\begin{enumerate}" : l ++ [text "\\end{enumerate}"]
