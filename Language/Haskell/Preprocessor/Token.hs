{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Preprocessor.Token (
  Token(..), Tag(..), newToken
) where

import Language.Haskell.Preprocessor.Loc
import Data.Typeable ()
import Data.Generics

data Token = Token { tag :: Tag,
                     com :: [Token],
                     loc :: Loc,
                     val :: String }
  deriving (Eq, Typeable, Data)

newToken :: Token
newToken = Token {
             tag = Other,
             com = [],
             loc = bogus,
             val = ""
           }

data Tag =
    CPragma
  | Variable
  | Constructor
  | Operator
  | Other
  | CharLit
  | StringLit
  | IntLit
  | FloatLit
  | VIndent
  | VDedent
  | VSemi
  | Comment
  | Error
  deriving (Eq, Show, Typeable, Data)

instance Show Token where
  showsPrec _ t = shows (tag t) . ('(':) . shows (val t) . (')':)

instance Locatable Token where
  getLoc     = loc
  setLoc t l = t { loc = l }

