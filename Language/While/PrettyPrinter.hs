{-# LANGUAGE FlexibleInstances #-}

-- https://hackage.haskell.org/package/pretty-1.1.3.2/docs/Text-PrettyPrint.html

module Language.While.PrettyPrinter where

import Text.PrettyPrint as PP
import Language.While.Types as W

class Pretty a where
  pretty :: a -> Doc
  plvl   :: Int -> a -> Doc
  prettyPrefix :: Doc -> a -> Doc
  prettySufix  :: Doc -> a -> Doc
  
  pretty = plvl 0
  plvl n = (nest n) . pretty
  prettyPrefix p = (p <>) . pretty
  prettySufix  p = (<> p) . pretty

-- | While Language Pretty Printer
  
instance (Pretty a) => Pretty (Maybe a) where
  pretty (Just a)   = pretty a
  pretty (Nothing)  = empty 
  
  prettyPrefix p (Just a) = p <> pretty a
  prettyPrefix _ Nothing  = empty
  
  prettySufix  p (Just a) = pretty a <> p
  prettySufix  _ Nothing  = empty
  
instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left x)  = pretty x
  pretty (Right x) = pretty x
  
instance Pretty Doc where
  pretty = id

-- | Abreviations

l = text "("
r = text ")"

instance Pretty Varname where
  pretty x = text x

instance Pretty Aexp where
  pretty (Numeral i)  = text $ show i
  pretty (Variable n) = pretty n
  pretty (Aadd e1 e2) = l <> (pretty e1) <> (text " + ") <> (pretty e2) <> r
  pretty (Asub e1 e2) = l <> (pretty e1) <> (text " - ") <> (pretty e2) <> r
  pretty (Amul e1 e2) = l <> (pretty e1) <> (text " * ") <> (pretty e2) <> r
  pretty (Adiv e1 e2) = l <> (pretty e1) <> (text " / ") <> (pretty e2) <> r

instance Pretty Bexp where
  pretty Btrue        = text "true"
  pretty Bfalse       = text "false"
  pretty (Beq e1 e2)  = l <> (pretty e1) <> (text " = ") <> (pretty e2) <> r
  pretty (Bleq e1 e2) = l <> (pretty e1) <> (text " <= ") <> (pretty e2) <> r
  pretty (Bl e1 e2)   = l <> (pretty e1) <> (text " < ") <> (pretty e2) <> r
  pretty (Bg e1 e2)   = l <> (pretty e1) <> (text " > ") <> (pretty e2) <> r
  pretty (Bgeq e1 e2) = l <> (pretty e1) <> (text " >= ") <> (pretty e2) <> r
  pretty (Bneg e1)    = l <> (text "!") <> (pretty e1) <> r
  pretty (Band e1 e2) = l <> (pretty e1) <> (text " ^ ") <> (pretty e2) <> r

instance Pretty Stm where
  pretty (Sass n e)         = (pretty n) <> (text " := ") <> (pretty e)
  pretty Sskip              = text "skip"
  pretty (Sassume b)        = text "assume " <> (pretty b)
  pretty (Sassert b)        = text "assert " <> (pretty b)
  pretty (Scomp s1 s2)      = (pretty s1) <> (text ";") $$ 
                              (pretty s2)
  pretty (Sif b st sf)      = (text "if") <+> (pretty b) <+> (text "then") $$
                              (nest 2 $ pretty st) $$ 
                              (text "else") $$
                              (nest 2 $ pretty sf) $$
                              (text "end")
  pretty (Swhile b s)       = (text "while") <+> (pretty b) <+> (text "do") $$
                              (nest 2 $ pretty s) $$
                              (text "od")
  pretty (SwhileInv b i s)  = (text "while") <+> (pretty b) 
                                             <+> (text "do") 
                                             <+> (text "{")
                                             <>  (pretty i) 
                                             <>  (text "}") $$
                              (nest 2 $ pretty s) $$
                              (text "od")
  pretty (Stry s1 s2)       = (text "try") $$
                              (nest 2 $ pretty s1) $$
                              (text "catch") $$
                              (nest 2 $ pretty s2)    