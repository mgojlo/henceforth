module Main (main) where

import Control.Exception
import Repl
import System.IO

main :: IO ()
main = do
  defs <- prelude
  maybe repl replWithPreloads defs
  where
    prelude = do
      catch (Just <$> readFile "./prelude.hf") exHandler
    exHandler :: IOError -> IO (Maybe String)
    exHandler e = do
      hPutStrLn stderr $ "Error reading prelude: " ++ show e
      hPutStrLn stderr "No prelude found, starting with empty environment"
      pure Nothing
