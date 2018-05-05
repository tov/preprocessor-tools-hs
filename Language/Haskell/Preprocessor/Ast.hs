{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Preprocessor.Ast (
  Ast(..), flatten, flattenList, format, cons,
  module Language.Haskell.Preprocessor.Token
) where

import Data.Typeable (Typeable)
import Data.Generics (Data, mkT, everywhere)
import Data.Char (isDigit)

import Language.Haskell.Preprocessor.Loc (Locatable(..), bogus)
import Language.Haskell.Preprocessor.Token

data Ast =
    Single { item   :: Token }
  | Block  { item   :: Token,
             lbrace :: Maybe Token,
             body   :: [Ast],
             rbrace :: Maybe Token,
             next   :: Ast }
  | Empty
  deriving (Eq, Show, Typeable, Data)

flatten              :: Ast -> [Token] -> [Token]
flatten (Single item) = (com item ++) . (item { com = [] }:)
flatten Empty         = id
flatten (Block item lbrace body rbrace next)
                      = (item :) .
                        maybe id (:) lbrace .
                        flattenList body .
                        maybe id (:) rbrace .
                        flatten next

flattenList          :: [Ast] -> [Token] -> [Token]
flattenList lst = foldr (.) id (map flatten lst)

format :: Data a => a -> [Ast] -> a
format a subs = everywhere (mkT replace) a where
  replace (Single Token { tag = CharLit,
                          val = '\'':'#':rest@(_:_) })
    | all isDigit num && index < length subs
      = subs !! index where
          num   = init rest
          index = read num - 1
  replace t = t

cons :: Token -> Ast -> Ast
cons a b = Block a Nothing [b] Nothing Empty

instance Locatable Ast where
  getLoc Empty   = bogus
  getLoc ast     = loc (item ast)

  setLoc Empty _ = Empty
  setLoc ast   l = ast { item = setLoc (item ast) l }
