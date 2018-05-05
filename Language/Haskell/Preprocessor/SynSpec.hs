module Language.Haskell.Preprocessor.SynSpec (
  SynSpec(..), Keyword(..),
  pair, defaultSpec
) where

import Data.Monoid (Monoid(..))

data SynSpec = SynSpec {
                 unboxed   :: Bool,
                 pragmas   :: Bool,
                 levelnest :: Bool,
                 blocks    :: [[Keyword]]
               }
  deriving (Eq, Show)

instance Semigroup SynSpec where
  s1 <> s2 = SynSpec {
               unboxed   = unboxed s1   || unboxed s2,
               pragmas   = pragmas s1   || pragmas s2,
               levelnest = levelnest s1 || levelnest s2,
               blocks    = blocks s1    ++ blocks s2
             }

instance Monoid SynSpec where
  mempty          = SynSpec {
                      unboxed   = False,
                      pragmas   = False,
                      levelnest = False,
                      blocks    = []
                    }

data Keyword = I { getKey :: String }
             | P { getKey :: String }
  deriving (Eq, Show)

pair    :: String -> String -> [Keyword]
pair l r = [P l, P r]

defaultSpec :: SynSpec
defaultSpec = SynSpec {
                unboxed   = True,
                pragmas   = False,
                levelnest = False,
                blocks    = [
                  pair "(#" "#)",
                  pair "{-#" "#-}",
                  pair "(" ")",
                  pair "[" "]",
                  pair "{" "}",
                  [P "if", P "then", P "else"],
                  [P "case", I "of"],
                  [I "let", P "in"],
                  [I "let"],
                  [I "where"],
                  [I "do"]
                ]
              }
