{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Preprocessor.Error (
  Error(..), fromParseError, errorAt
) where

import Text.ParserCombinators.Parsec.Error
import Data.Typeable (Typeable)
import Data.Generics (Data)

import Language.Haskell.Preprocessor.Loc

data Error = Error { loc :: Loc,
                     msg :: String }
  deriving (Typeable, Data)

errorAt :: Locatable a => a -> String -> b
errorAt a m = error (show (Error (getLoc a) m))

fromParseError :: ParseError -> Error
fromParseError pe =
  Error { loc = fromSourcePos (errorPos pe),
          msg = indent 4 $
                showErrorMessages
                  "or" "Unknown" "Expecting"
                  "Unexpected" "end-of-input"
                  (errorMessages pe) }
    where indent n = unlines . map (replicate n ' ' ++) . lines

instance Show Error where
  showsPrec _ (Error loc msg)
    | isBogus loc = ("Error: "++) . (msg++)
    | otherwise   = ("At "++) . shows loc . (": "++) . (msg ++)

