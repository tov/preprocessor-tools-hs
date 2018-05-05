module Language.Haskell.Preprocessor (
  module Language.Haskell.Preprocessor.Ast,
  module Language.Haskell.Preprocessor.Parser,
  module Language.Haskell.Preprocessor.Printer,
  module Language.Haskell.Preprocessor.SynSpec,
  module Language.Haskell.Preprocessor.Util,
  module Data.Monoid,
  Loc.Locatable(..), Loc.cloneLoc, Loc.scrub,
  Extension(..), base, transform,
  hLoad, fileLoad, stdinLoad,
  hDump, fileDump, stdoutDump, stringDump
) where

import System.IO
import System.Environment (getProgName)
import System.Exit (exitFailure)
import Control.Exception (bracket)
import Control.Monad ((<=<))

import Data.Monoid (Monoid(..))
import qualified Control.Monad.Writer as W

import qualified Language.Haskell.Preprocessor.Loc   as Loc
import qualified Language.Haskell.Preprocessor.Error as E
import Language.Haskell.Preprocessor.Ast
import Language.Haskell.Preprocessor.Parser
import Language.Haskell.Preprocessor.Printer
import Language.Haskell.Preprocessor.SynSpec
import Language.Haskell.Preprocessor.Util

data Extension = Extension {
                   keywords    :: [[Keyword]],
                   transformer :: [Ast] -> IO [Ast],
                   synspec     :: SynSpec,
                   usage       :: Maybe (IO ()),
                   syntaxerror :: Maybe (E.Error -> IO ())
                 }

instance Semigroup Extension where
  e1 <> e2 = Extension {
               keywords    = keywords e1 ++ keywords e2,
               transformer = transformer e1 <=< transformer e2,
               synspec     = synspec e1 `mappend` synspec e2,
               usage       = usage e1       <+ usage e2,
               syntaxerror = syntaxerror e1 <+ syntaxerror e2
             }
    where Just a  <+ _ = Just a
          Nothing <+ b = b

instance Monoid Extension where
  mempty          = Extension {
                      keywords    = [],
                      transformer = return,
                      synspec     = mempty,
                      usage       = Nothing,
                      syntaxerror = Nothing
                    }

base :: Extension
base  = Extension {
          keywords    = [],
          transformer = return,
          synspec     = defaultSpec,
          usage       = Just (do
            prog <- getProgName
            hPutStrLn stderr $
              "Usage: "++prog++" [ INFILE | SOURCE INFILE OUTFILE ]"),
          syntaxerror = Just (hPutStrLn stderr . show)
        }

transform :: Extension -> [String] -> IO ()
transform extension files = do
    easts <- case files of
      []     -> stdinLoad spec
      [file] -> fileLoad spec file file
      [source, file, _]
             -> fileLoad spec source file
      _      -> do case usage extension of
                     Just m  -> m
                     Nothing -> return ()
                   exitFailure
    asts  <- case easts of
      Left e  -> do case syntaxerror extension of
                      Just m  -> m e
                      Nothing -> return ()
                    exitFailure
      Right r -> return r
    result <- transformer extension asts
    case files of
      [_, _, file] -> fileDump spec file result
      _            -> stdoutDump spec result
  where
    spec = (synspec extension) {
             blocks = keywords extension ++ blocks (synspec extension)
           }

-- Loading

hLoad :: SynSpec -> String -> Handle -> IO (Either E.Error [Ast])
hLoad spec source handle = do
  input <- hGetContents handle
  return (parseBy spec source input)

fileLoad :: SynSpec -> String -> FilePath -> IO (Either E.Error [Ast])
fileLoad spec source filename = do
  input <- readFile filename
  return (parseBy spec source input)

stdinLoad :: SynSpec -> IO (Either E.Error [Ast])
stdinLoad spec = hLoad spec "-" stdin

-- Dumping

hDump        :: SynSpec -> Handle -> [Ast] -> IO ()
hDump _       = dump . hPutStr

stringDump   :: SynSpec -> [Ast] -> String
stringDump _  = W.execWriter . dump W.tell

fileDump     :: SynSpec -> String -> [Ast] -> IO ()
fileDump spec filename ast =
  bracket (openFile filename WriteMode) hClose $ \handle ->
    hDump spec handle ast

stdoutDump   :: SynSpec -> [Ast] -> IO ()
stdoutDump _  = dump putStr

