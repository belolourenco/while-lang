{-# LANGUAGE FlexibleInstances #-}

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
  pretty (Bneg e1)    = l <> (text "!") <> (pretty e1) <> r
  pretty (Band e1 e2) = l <> (pretty e1) <> (text " ^ ") <> (pretty e2) <> r

instance Pretty Stm where
  pretty (Sass n e)     = (pretty n) <> (text " := ") <> (pretty e)
  pretty Sskip          = text "skip"
  pretty (Scomp s1 s2)  = (pretty s1) <> (text ";") $$ 
                          (pretty s2)
  pretty (Sif b st sf)  = (text "if") <+> (pretty b) <+> (text "then") $$
                          (nest 2 $ pretty st) $$ 
                          (text "else") $$
                          (nest 2 $ pretty sf)
  pretty (Swhile b s)   = (text "while") <+> (pretty b) <+> (text "do") $$
                          (nest 2 $ pretty s)
  pretty (Stry s1 s2)   = (text "try") $$
                          (nest 2 $ pretty s1) $$
                          (text "catch") $$
                          (nest 2 $ pretty s2)