{-# LANGUAGE FlexibleContexts #-}

module Repl (repl, replWithPreloads) where

import Builtin
import Control.Monad
import Ctx
import CtxTypes
import Data.Char
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe
import Eval
import Parse
import System.IO

type Cont a b = (a -> b) -> b

repl :: IO ()
repl = repl' $ newCtx builtins

info :: IO ()
info = do
  hPutStrLn stderr "#s: print the current stack"
  hPutStrLn stderr "#d <name>: print the definition of <name>"
  hPutStrLn stderr "#t <name>: print the type of <name>"
  hPutStrLn stderr "#e: print all definitions"
  hPutStrLn stderr "#S: try to print type of the stack"
  hPutStrLn stderr "#rs: reset stack"

replWithPreloads :: String -> IO ()
replWithPreloads s = do
  let ctx = newCtx builtins
  replCore s ctx repl'

replCore :: String -> Ctx -> Cont Ctx (IO ())
replCore s ctx k = do
  case parse s of
    Left err -> hPutStrLn stderr ("Parse error: " ++ err) >> k ctx
    Right ast -> case eval ast ctx of
      Left err -> hPutStrLn stderr ("Eval error: " ++ err) >> k ctx
      Right ctx' -> k ctx'

repl' :: Ctx -> IO ()
repl' ctx = do
  hPutStr stderr "> "
  hFlush stderr
  eof <- isEOF
  guard $ not eof
  s <- getLine
  if null s
    then repl' ctx
    else
      if "#" `isPrefixOf` s
        then do
          let (ctx', eff) = handlePragmas s ctx
          eff
          repl' ctx'
        else replCore s ctx repl'

handlePragmas :: String -> Ctx -> (Ctx, IO ())
handlePragmas s ctx
  | s == "#?" = (ctx, info)
  | s == "#help" = (ctx, info)
  | s == "#s" = (ctx, hPrint stderr (getStack ctx))
  | s == "#S" = (ctx, hPrint stderr $ stackTp (getStack ctx) (getTEnv ctx))
  | s == "#rs" = (replaceStack [] ctx, pure ())
  | "#d " `isPrefixOf` s =
      let n = trim $ drop 3 s
       in (ctx, hPutStrLn stderr $ maybe "(undefined)" show (M.lookup n (getDEnv ctx)))
  | "#t " `isPrefixOf` s =
      let n = trim $ drop 3 s
       in (ctx, hPutStrLn stderr $ maybe "(undefined)" show (M.lookup n (getTEnv ctx)))
  | "#e" `isPrefixOf` s = do
      (ctx, mapM_ (hPrint stderr) (M.toList (getTEnv ctx)))
  | otherwise = (ctx, hPutStrLn stderr "Unknown pragma")

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
