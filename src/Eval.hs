{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Eval where

import AST
import Control.Monad.Except
import Ctx
import CtxTypes
import qualified Data.Map as M
import Debug.Trace
import Inference
import InferenceTypes

eval :: [AST] -> Ctx -> Either String Ctx
eval ast ctx = do
  _ <- typecheck ast ctx
  ctx' <- eval' ast ctx
  let !_ = forceSt ctx'
  pure ctx'

forceSt :: Ctx -> ()
forceSt ctx = go (getStack ctx)
  where
    go [] = ()
    go (!x : xs) = go xs

stackTp :: Stack -> TEnv Ident -> Tp Ident
stackTp st env = TFn [] (aux st env)
  where
    aux :: Stack -> TEnv Ident -> [Tp Ident]
    aux [] _ = []
    aux (VInt _ : xs) e = TBase TInt : aux xs e
    aux (VFun (Interp _ tp _ _) : xs) e = tp : aux xs e
    aux (VFun (Builtin _ (Mono tp)) : xs) e = tp : aux xs e
    -- FIXME
    aux (VFun (Builtin _ (Poly (Forall _binds tp))) : xs) e = tp : aux xs e

typecheck :: [AST] -> Ctx -> Either String (Tp Ident, TEnv Ident)
typecheck ast ctx = do
  let stp = stackTp (getStack ctx) (getTEnv ctx)
  (tp, env) <- typeof (getTEnv ctx) ast
  -- To nie działa :) :(
  -- _ <- tryMatchTypes stp tp
  pure (tp, env)

define :: String -> [AST] -> Ctx -> Either String Ctx
define name ast ctx = do
  (tp, env) <- typecheck ast ctx
  let ctx' = insertTEnv (replaceTEnv env ctx) name (generalize env tp)
  pure $ insertDEnv name (Interp ast tp (getDEnv ctx') env) ctx'

eval' :: [AST] -> Ctx -> Either String Ctx
eval' [] ctx = pure ctx
eval' (ADefn n body : rest) ctx = do
  ctx' <- define n body ctx
  eval' rest ctx'
eval' (ACall n : rest) ctx = do
  case M.lookup n (getDEnv ctx) of
    Nothing -> throwError $ "eval': Unknown function: '" ++ n ++ "'"
    Just (Builtin f _tp) -> let ctx' = f ctx in eval' rest ctx'
    Just (Interp is _tp denv _tenv) -> do
      let denv_saved = getDEnv ctx
      ctx' <- eval' is (replaceDEnv denv ctx)
      eval' rest (replaceDEnv denv_saved ctx')
eval' (AIntL i : rest) ctx = eval' rest (push (VInt i) ctx)
eval' (ALamb is : rest) ctx = do
  (tp, tenv) <- typecheck is ctx
  eval' rest (push (VFun (Interp is tp (getDEnv ctx) tenv)) ctx)
eval' (AComm _ : rest) ctx = eval' rest ctx
