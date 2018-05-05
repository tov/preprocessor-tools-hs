module Language.Haskell.Preprocessor.Util (
  parens, noParens,
  splitVal, splitTag, splitSemis, splitAllBy, splitBy,
  valIs, tagIs
) where

import Language.Haskell.Preprocessor.Ast

parens, noParens :: [Ast] -> Ast
parens asts = Block {
    item   = newToken { tag = Other, val = "(" },
    lbrace = Nothing,
    body   = asts,
    rbrace = Nothing,
    next   = Single newToken { tag = Other, val = ")" }
  }
noParens asts = Block {
    item   = newToken { tag = Other, val = "" },
    lbrace = Nothing,
    body   = asts,
    rbrace = Nothing,
    next   = Single newToken { tag = Other, val = "" }
  }

splitVal :: String -> [Ast] -> Maybe ([Ast], Ast, [Ast])
splitVal = splitBy . valIs

splitTag :: Tag -> [Ast] -> Maybe ([Ast], Ast, [Ast])
splitTag = splitBy . tagIs

splitSemis :: [Ast] -> [[Ast]]
splitSemis = filter (not . null) . splitAllBy isSemi where
  isSemi t = valIs ";" t || tagIs VSemi t

splitAllBy :: (Ast -> Bool) -> [Ast] -> [[Ast]]
splitAllBy _    [] = []
splitAllBy pred lst = case splitBy pred lst of
  Nothing        -> [lst]
  Just (f, _, b) -> f : splitAllBy pred b

splitBy :: (Ast -> Bool) -> [Ast] -> Maybe ([Ast], Ast, [Ast])
splitBy pred lst = case lst of
  []               -> Nothing
  x:xs | pred x    -> Just ([], x, xs)
       | otherwise -> do
           (front, middle, back) <- splitBy pred xs
           return (x:front, middle, back)

valIs :: String -> Ast -> Bool
valIs v Single { item = t } = val t == v
valIs _ _                   = False

tagIs :: Tag -> Ast -> Bool
tagIs v Single { item = t } = tag t == v
tagIs _ _                   = False

