module Language.Haskell.Preprocessor.Parser (
  quasi, quasiBy, parse, parseBy, parseTokens, parseTokensBy
) where

import Data.List (delete, union)

import qualified Language.Haskell.Preprocessor.SynSpec as SS
import qualified Language.Haskell.Preprocessor.Error   as E
import qualified Language.Haskell.Preprocessor.Loc     as Loc
import qualified Language.Haskell.Preprocessor.Lexer   as Lexer
import Language.Haskell.Preprocessor.Ast

import qualified Text.ParserCombinators.Parsec as P

quasiBy :: SS.SynSpec -> String -> [Ast] -> [Ast]
quasiBy spec input subs =
  case parseBy spec "<quasi>" input of
    Left e  -> error (show e)
    Right r -> format (Loc.scrub r) subs

quasi :: String -> [Ast] -> [Ast]
quasi = quasiBy SS.defaultSpec

parse :: String -> String -> Either E.Error [Ast]
parse  = parseBy SS.defaultSpec

parseBy :: SS.SynSpec -> String -> String -> Either E.Error [Ast]
parseBy spec file input = parseTokensBy spec tokens where
  tokens = Lexer.scan spec file input

parseTokens :: [Token] -> Either E.Error [Ast]
parseTokens  = parseTokensBy SS.defaultSpec

type Grammar = [Rule]
data Rule = Branch SS.Keyword Grammar
          | Epsilon
  deriving (Eq, Show)

leftFactor :: [[SS.Keyword]] -> Grammar
leftFactor  = foldr add [] where
  add :: [SS.Keyword] -> Grammar -> Grammar
  add []     gram = delete Epsilon gram ++ [Epsilon]
  add (kw:r) gram = case findSplit (headIs kw) gram of
    Just (a, Branch kw' r', c)
            -> Branch (max kw kw') (add r r') : a ++ c
    _       -> Branch kw (add r []) : gram

  max (SS.I kw) _ = SS.I kw
  max _ (SS.I kw) = SS.I kw
  max x _         = x

  headIs kw (Branch kw' _) = SS.getKey kw == SS.getKey kw'
  headIs _  _              = False

  findSplit _    [] = Nothing
  findSplit pred (x:xs)
    | pred x        = Just ([], x, xs)
    | otherwise     = do (a, b, c) <- findSplit pred xs
                         Just (x:a, b, c)

parseTokensBy :: SS.SynSpec -> [Token] -> Either E.Error [Ast]
parseTokensBy spec input = case P.parse start source input of
    Left pe    -> Left (E.fromParseError pe)
    Right asts -> Right asts
  where

  gram = leftFactor (SS.blocks spec)

  start = do
    res <- until []
    P.eof
    return res

  source = case input of
    t:_ -> Loc.file (loc t)
    _   -> "<bogus>"

  until stop = P.many (any stop)

  any stop   = parseRules gram (single stop) stop

  single stop = do
    item <- tokenP (\i -> val i `notElem` stop && tag i /= VDedent)
    return (Single item)

  parseRules r orelse stop =
    foldr (<|>) orelse (map (flip parseRule stop) r)

  parseRule Epsilon _                           = return Empty
  parseRule (Branch (SS.I key) r) stop          = do
    item <- tokenV key
    curlyBraces item r stop <|> virtualBraces item r stop
  parseRule (Branch (SS.P key) [Epsilon]) _     = do
    item <- tokenV key
    return (Single item)
  parseRule (Branch (SS.P key) r) stop          = do
    item <- tokenV key
    body <- until (follow stop r)
    next <- parseRules r P.pzero stop           <??> r
    return (Block item Nothing body Nothing next)

  curlyBraces item r stop                       = do
    lbrace <- tokenV "{"                        <?> "{"
    body   <- until ["}"]
    rbrace <- tokenV "}"                        <?> "}"
    next   <- parseRules r P.pzero stop         <??> r
    return (Block item (Just lbrace) body (Just rbrace) next)

  virtualBraces item r stop                     = do
    lbrace <- tokenT VIndent
    body   <- until (follow stop r)
    rbrace <- do
                vbrace <- tokenT VDedent
                return (Just vbrace)
          <|> do
                killDedent
                return Nothing
    next   <- parseRules r P.pzero stop         <??> r
    return (Block item (Just lbrace) body rbrace next)

  follow oldStop r
    | Epsilon `elem` r = oldStop `union` newStop
    | otherwise        = newStop
    where newStop = [ SS.getKey kw | Branch kw _ <- r ]

  killDedent = do
      inp <- P.getInput
      P.setInput (clean 0 inp)
    where
      clean     :: Int -> [Token] -> [Token]
      clean _ [] = []
      clean 0 (  Token { tag = VDedent }:r) = r
      clean n (t@Token { tag = VIndent }:r) = t : clean (n + 1) r
      clean n (t@Token { tag = VDedent }:r) = t : clean (n - 1) r
      clean n (t                        :r) = t : clean n r

  tokenRaw :: (Token -> Maybe a) -> P.GenParser Token () a
  tokenRaw = P.token val (Loc.toSourcePos . loc)

  token :: (Token -> Maybe a) -> P.GenParser Token () a
  token pred = tokenRaw pred <|> errorToken

  tokenP :: (Token -> Bool) -> P.GenParser Token () Token
  tokenP pred = token (\i -> if pred i then Just i else Nothing)

  tokenV :: String -> P.GenParser Token () Token
  tokenV v = tokenP ((== v) . val)

  tokenT :: Tag -> P.GenParser Token () Token
  tokenT t = tokenP ((== t) . tag)

  errorToken :: P.GenParser Token () a
  errorToken = do
    tok <- tokenRaw (\i -> if tag i == Error then Just i else Nothing)
    P.setPosition (Loc.toSourcePos (loc tok))
    fail (val tok)

  (<|>) = (P.<|>)
  (<?>) = (P.<?>)

  p <??> []              = p
  p <??> (Epsilon:_)     = p
  p <??> (Branch kw _:r) = (p <?> SS.getKey kw) <??> r

