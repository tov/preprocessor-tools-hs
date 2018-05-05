module Language.Haskell.Preprocessor.Lexer (
  scan, dscan, module T
) where

import Data.Char

import Language.Haskell.Preprocessor.Token as T
import Language.Haskell.Preprocessor.Loc
import qualified Language.Haskell.Preprocessor.SynSpec as SS

scan           :: SS.SynSpec -> String -> String -> [Token]
scan spec file  = pass3 spec . pass2 spec . pass1 spec file

dscan          :: String -> String -> [Token]
dscan           = scan SS.defaultSpec

lexerr         :: Loc -> String -> [Token]
lexerr loc msg  = [newToken { tag = Error, T.loc = loc, val = msg }]

pass1 :: SS.SynSpec -> String -> String -> [Token]
pass1 spec file str = bol (initial file) str where
  unboxed = SS.unboxed spec
  pragmas = SS.pragmas spec

  varinit c = isLower c || c == '_'
  coninit c = isUpper c
  opinit  c = c `elem` ":!$%&*+./<=>?@\\^|-~"

  varcont c = isAlphaNum c || c `elem` "'_"
  concont   = varcont
  opcont  c = opinit c || c == '#'

  rawnext tag loc inp val = loc' `seq` new : token loc' inp where
    new  = newToken { T.tag = tag, T.loc = loc, T.val = val }
    loc' = advance loc val

  next  tag loc inp val = rawnext tag loc inp val
  rnext tag loc inp val = next tag loc inp (reverse val)

  bol loc inp = case inp of
    '#':r -> let (acc, inp) = eatWhile (/= '\n') r "#"
                 val        = reverse acc in
             case fromDirective val of
               Nothing  -> next CPragma loc inp val
               Just loc ->
                 case inp of
                   '\n':r -> bol loc r
                   _      -> []
    c:inp | isSpace c -> bol (advance loc c) inp
    _ -> token loc inp

  token loc inp = case inp of
    '(':'#':r     | unboxed -> next Other loc r "(#"
    '#':')':r     | unboxed -> next Other loc r "#)"
    '{':'-':'#':r | pragmas -> next Other loc r "{-#"
    '#':'-':'}':r | pragmas -> next Other loc r "#-}"
    '{':'-':r -> lexComment loc r "-{" 1
    '(':r     -> next Other loc r "("
    ')':r     -> next Other loc r ")"
    '[':r     -> next Other loc r "["
    ']':r     -> next Other loc r "]"
    '{':r     -> next Other loc r "{"
    '}':r     -> next Other loc r "}"
    ';':r     -> next Other loc r ";"
    ',':r     -> next Other loc r ","
    '`':r     -> next Other loc r "`"
    '\'':r    -> lexChar loc r "'"
    '"':r     -> lexString loc r "\""
    '-':'-':r -> lexHyphen loc r "--"
    '\n':r -> bol (advance loc '\n') r
    c:r | isSpace c -> token (advance loc c) r
        | coninit c -> lexCon loc r [c]
        | varinit c -> lexVar loc r [c]
        | opinit c  -> lexOp loc r [c]
        | isDigit c -> lexNumber loc r [c]
    c:_    -> lexerr loc ("unexpected character " ++ show c)
    []     -> []

  lexChar   loc inp acc = case inp of
    '\'':'#':r
      | unboxed -> rnext CharLit loc r ('#':'\'':acc)
    '\'':r      -> rnext CharLit loc r ('\'':acc)
    '\\':'\'':r -> lexChar loc r ('\'':'\\':acc)
    '\\':'\\':r -> lexChar loc r ('\\':'\\':acc)
    c:r         -> lexChar loc r (c:acc)
    []          -> lexerr loc "eof in character literal"

  lexString loc inp acc = case inp of
    '"':'#':r
      | unboxed -> rnext StringLit loc r ('#':'"':acc)
    '"':r       -> rnext StringLit loc r ('"':acc)
    '\\':'"':r  -> lexString loc r ('"':'\\':acc)
    '\\':'\\':r -> lexString loc r ('\\':'\\':acc)
    c:r         -> lexString loc r (c:acc)
    []          -> lexerr loc "eof in string literal"

  lexNumber loc inp acc = case inp of
    c:r | isDigit c     -> lexNumber loc r (c:acc)
        | c == '.' && not (null r) && isDigit (head r)
                        -> lexFloat loc r (c:acc)
        | c `elem` "eE" -> lexMaybeExp IntLit loc inp acc
    '#':r | unboxed     -> rnext IntLit loc r ('#':acc)
    _                   -> rnext IntLit loc inp acc

  lexFloat loc inp acc = case eatWhile isDigit inp acc of
    (acc, r@(c:_)) | c `elem` "eE" -> lexMaybeExp FloatLit loc r acc
    (acc, '#':r) | unboxed         -> rnext FloatLit loc r ('#':acc)
    (acc, r)                       -> rnext FloatLit loc r acc

  lexMaybeExp tag loc inp acc = case inp of
    e:'-':d:r | isDigit d -> lexExp loc r (d:'-':e:acc)
    e:'+':d:r | isDigit d -> lexExp loc r (d:'+':e:acc)
    e:d:r     | isDigit d -> lexExp loc r (d:e:acc)
    _                     -> rnext tag loc inp acc

  lexExp loc inp acc = case eatWhile isDigit inp acc of
    (acc, '#':r) -> rnext FloatLit loc r ('#':acc)
    (acc, r)     -> rnext FloatLit loc r acc

  lexVar    loc inp acc = case inp of
    c:r | varcont c -> lexVar loc r (c:acc)
    '#':r | unboxed -> rnext Variable loc r ('#':acc)
    _               -> rnext Variable loc inp acc

  lexOp     loc inp acc = case inp of
    c:r | opcont c -> lexOp loc r (c:acc)
    _              -> rnext Operator loc inp acc

  lexCon    loc inp acc = case inp of
    c:r | concont c -> lexVar loc r (c:acc)
    '#':r | unboxed -> rnext Constructor loc r ('#':acc)
    '.':r -> case r of
               c:s | coninit c -> lexCon loc s (c:'.':acc)
                   | varinit c -> lexVar loc s (c:'.':acc)
                   | opcont c  -> lexOp loc s (c:'.':acc)
               _ -> rnext Constructor loc inp acc
    _ -> rnext Constructor loc inp acc

  lexHyphen loc inp acc = case inp of
    '-':r           -> lexHyphen loc r ('-':acc)
    c:r | opcont c  -> lexOp loc r (c:acc)
    _               -> case eatWhile (/= '\n') inp acc of
                         (acc, r) -> rnext Comment loc r acc

  lexComment :: Loc -> String -> String -> Int -> [Token]
  lexComment loc inp acc 0 = rnext Comment loc inp acc
  lexComment loc inp acc n = case inp of
    '-':'}':r -> lexComment loc r ('}':'-':acc) (n - 1)
    '{':'-':r -> lexComment loc r ('-':'{':acc) (n + 1)
    c:r       -> lexComment loc r (c:acc) n
    []        -> lexerr loc "eof in comment"

  eatWhile pred inp acc = case inp of
    c:r | pred c  -> eatWhile pred r (c:acc)
    _             -> (acc, inp)

pass2 :: SS.SynSpec -> [Token] -> [Token]
pass2 _ = loop [] where
  loop acc toks = case toks of
    t@Token { tag = Comment } : rest
       -> loop (t:acc) rest
    t : rest
       -> t { com = reverse acc } : loop [] rest
    [] -> case acc of
      []         -> []
      t:_ -> [newToken { tag = Other,
                         com = reverse acc,
                         loc = loc t `advance` '\n'} ]

pass3 :: SS.SynSpec -> [Token] -> [Token]
pass3 spec = loop [] where
  levelnest = SS.levelnest spec
  indents   = [ s | block <- SS.blocks spec, SS.I s <- block ]

  indent loc = Token VIndent [] loc ""
  semi   loc = Token VSemi   [] loc ""
  dedent loc = Token VDedent [] loc ""

  loop stk toks = case toks of
    []   -> []
    t:ts
      | val t `elem` indents ->
          case ts of
            []   -> t : indent _loc : dedent _loc : check _loc stk []
                    where _loc = loc t
            t':_
              | val t' == "{" ->
                  t : check (loc t') stk ts
              | otherwise     ->
                  t : indent (loc t') : begin (loc t') stk ts
      | otherwise  -> t : check (loc t) stk ts

  begin _loc stk toks = case stk of
      []        -> loop [n] toks
      m:_ -> case n `compare` m of
        GT      -> loop (n:stk) toks
        EQ | levelnest
                -> loop (n:stk) toks
        _       -> dedent _loc : check _loc stk toks
    where n = col _loc

  check _loc stk toks = case (stk, toks) of
    (ms,   [])   -> map (const (dedent _loc)) ms
    (m:ms, t:ts) ->
      case col (loc t) `compare` m of
        LT -> dedent (loc t) : check (loc t) ms (t:ts)
        EQ -> semi (loc t) : loop (m:ms) (t:ts)
        _  -> loop (m:ms) (t:ts)
    ([],   ts) -> loop [] ts

