{-# LANGUAGE FlexibleContexts #-}

module Ctx where

import AST
import Control.Monad.Except
import CtxTypes
import qualified Data.Map as M
import InferenceTypes

newCtx std = ([], tenv, denv)
  where
    tenv = M.fromList $ map (\(n, t, _f) -> (n, t)) std
    denv = M.fromList $ map (\(n, t, f) -> (n, Builtin f (Poly t))) std

pop :: (MonadError String m) => ([b1], b2, c) -> m (([b1], b2, c), b1)
pop ([], _, _) = throwError "Stack underflow"
pop (v : vs, tenv, denv) = pure ((vs, tenv, denv), v)

push :: Value -> Ctx -> Ctx
push v (vs, tenv, denv) = (v : vs, tenv, denv)

getTEnv :: Ctx -> TEnv Ident
getTEnv (_, tenv, _) = tenv

replaceTEnv :: TEnv Ident -> Ctx -> Ctx
replaceTEnv tenv (s, _, denv) = (s, tenv, denv)

insertTEnv :: Ctx -> Ident -> Scheme Ident -> Ctx
insertTEnv (s, tenv, denv) i s' = (s, M.insert i s' tenv, denv)

getDEnv :: Ctx -> DEnv
getDEnv (_, _, denv) = denv

replaceDEnv :: DEnv -> Ctx -> Ctx
replaceDEnv denv' (s, tenv, _) = (s, tenv, denv')

insertDEnv :: Ident -> Fun -> Ctx -> Ctx
insertDEnv i f (s, tenv, denv) = (s, tenv, M.insert i f denv)

getStack :: Ctx -> Stack
getStack (s, _, _) = s

replaceStack :: Stack -> Ctx -> Ctx
replaceStack s (_, tenv, denv) = (s, tenv, denv)
