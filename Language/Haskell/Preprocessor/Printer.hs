module Language.Haskell.Preprocessor.Printer ( dump ) where

import Language.Haskell.Preprocessor.Ast
import Language.Haskell.Preprocessor.Loc

import Control.Monad (foldM, foldM_)

dump :: Monad m => (String -> m ()) -> [Ast] -> m ()
dump write forest = start where
  start = do
    let items = flattenList forest []
        here  = case items of
                  []  -> bogus
                  x:_ -> initial (file (loc x))
    write (toDirective here)
    write "\n"
    foldM_ sayToken here items
    write "\n"

  sayToken here item
    | null (val item) = do
        return here
    | isBogus (loc item) = do
        foldM sayString here [" ", val item, " "]
    | otherwise = do
        here <- goto here (loc item)
        sayString here (val item)

  goto here there
    | line here < line there = do
        let directive = toDirective there
            newlines  = line there - line here
        if length directive < newlines
          then do
            write "\n"; write directive; write "\n"
          else do
            write (replicate (line there - line here) '\n')
        write (replicate (col there - 1) ' ')
        return there
    | line here == line there && col here <= col there = do
        write (replicate (col there - col here) ' ')
        return there
    | otherwise = do
        write "\n"
        write (toDirective there)
        write "\n"
        write (replicate (col there - 1) ' ')
        return there

  sayString here str = do
    write str
    return (here `advance` str)
