{-# LANGUAGE FlexibleInstances #-}

module Language.WhileSA.PrettyPrinter where

import Text.PrettyPrint as PP
import Language.WhileSA.Types
import Language.While.PrettyPrinter

-- | WhileSA Language Pretty Printer

instance Pretty VarnameSA where
  pretty (n, v) = l <> pretty n 
                    <> text ", [" 
                    <> (hcat $ punctuate (text ",") (map (text.show) v))
                    <> text "]"
                    <> r

instance Pretty Rnm where
  pretty r = text "[" <> (hcat $ punctuate (text ",") (map aux r)) <> text "]"
    where aux (x,y) = pretty $ SassSA x (VariableSA y)

instance Pretty AexpSA where
  pretty (NumeralSA i)  = text $ show i
  pretty (VariableSA n) = pretty n
  pretty (AaddSA e1 e2) = l <> (pretty e1) <> (text " + ") <> (pretty e2) <> r
  pretty (AsubSA e1 e2) = l <> (pretty e1) <> (text " - ") <> (pretty e2) <> r
  pretty (AmulSA e1 e2) = l <> (pretty e1) <> (text " * ") <> (pretty e2) <> r
  pretty (AdivSA e1 e2) = l <> (pretty e1) <> (text " / ") <> (pretty e2) <> r

instance Pretty BexpSA where
  pretty BtrueSA        = text "true"
  pretty BfalseSA       = text "false"
  pretty (BeqSA e1 e2)  = l <> (pretty e1) <> (text " = ") <> (pretty e2) <> r
  pretty (BleqSA e1 e2) = l <> (pretty e1) <> (text " <= ") <> (pretty e2) <> r
  pretty (BnegSA e1)    = l <> (text "!") <> (pretty e1) <> r
  pretty (BandSA e1 e2) = l <> (pretty e1) <> (text " ^ ") <> (pretty e2) <> r

instance Pretty StmSA where
  pretty (SassSA n e)     = (pretty n) <> (text " := ") <> (pretty e)
  pretty SskipSA          = text "skip"
  pretty (ScompSA s1 s2)  = (pretty s1) <> (text ";") $$ 
                          (pretty s2)
  pretty (SifSA b st sf)  = (text "if") <+> (pretty b) <+> (text "then") $$
                          (nest 2 $ pretty st) $$ 
                          (text "else") $$
                          (nest 2 $ pretty sf)
  pretty (SforSA i b u s) = (text "for")  <> l
                                          <> pretty i
                                          <> text ","
                                          <> pretty b
                                          <> pretty u
                                          <> r
                                          <> (text "do") $$
                          (nest 2 $ pretty s)
  pretty (StrySA s1 s2)   = (text "try") $$
                          (nest 2 $ pretty s1) $$
                          (text "catch") $$
                          (nest 2 $ pretty s2)