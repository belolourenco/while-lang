module Language.WhileSA.Types where

import Language.While.Types

-- | Variables type alias.
type Version = [Integer]
type VarnameSA = (Varname,Version)

-- | Variable renaming.
type Rnm = [(VarnameSA,VarnameSA)]

-- | Arithmetic expressions, including integer division.
data AexpSA
  = NumeralSA Integer
  | VariableSA VarnameSA
  | AaddSA AexpSA AexpSA
  | AsubSA AexpSA AexpSA
  | AmulSA AexpSA AexpSA
  | AdivSA AexpSA AexpSA
  deriving (Show, Eq)


-- | Boolean expressions.
data BexpSA
  = BtrueSA
  | BfalseSA
  | BVariableSA VarnameSA
  | BeqSA AexpSA AexpSA
  | BleqSA AexpSA AexpSA
  | BlSA AexpSA AexpSA
  | BgSA AexpSA AexpSA
  | BgeqSA AexpSA AexpSA
  | BnegSA BexpSA
  | BandSA BexpSA BexpSA
  | BorSA BexpSA BexpSA
  | BimplSA BexpSA BexpSA
  deriving (Show, Eq)

-- | Statements, including try-catch clauses.
data StmSA
  = SassSA VarnameSA AexpSA
  | SskipSA
  | SassumeSA BexpSA
  | SassertSA BexpSA
  | ScompSA StmSA StmSA
  | SifSA BexpSA StmSA StmSA
  | SforSA Rnm BexpSA Rnm StmSA
  | SforInvSA Rnm BexpSA Rnm BexpSA StmSA
  | StrySA StmSA StmSA
  | SthrowA
  deriving (Show, Eq)
