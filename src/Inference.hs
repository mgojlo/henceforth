{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Inference where

import AST
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import InferenceTypes
import Util

class Substitutable v t | t -> v where
  freevars :: t -> S.Set v
  applySubst :: Subst v -> t -> t

instance (Ord v) => Substitutable v (Tp v) where
  freevars (TBase _) = S.empty
  freevars (TFn before after) = S.unions $ map freevars (before ++ after)
  freevars (TVar v) = S.singleton v
  applySubst _ tp@(TBase _) = tp
  applySubst subst (TFn before after) = TFn (map (applySubst subst) before) (map (applySubst subst) after)
  applySubst subst v@(TVar u) = fromMaybe v (M.lookup u subst)

instance (Ord v) => Substitutable v (Scheme v) where
  freevars (Forall binds tp) = S.difference (freevars tp) binds
  applySubst subst (Forall binds tp) = Forall binds (applySubst subst' tp)
    where
      subst' = foldr M.delete subst binds

instance (Ord v) => Substitutable v (TEnv v) where
  freevars env = S.unions $ M.map freevars env
  applySubst subst = M.map (applySubst subst)

generalize :: (Ord v) => TEnv v -> Tp v -> Scheme v
generalize env tp = Forall binds tp
  where
    binds = S.difference (freevars tp) (freevars env)

instantiate (Forall binds tp) = do
  let binds_l = S.toList binds
  newbinds <- mapM (const fresh) binds_l
  let subst = M.fromList $ zip binds_l newbinds
  pure $ applySubst subst tp

composeFns (TFn as []) (TFn cs ds) = pure $ TFn (as ++ cs) ds
composeFns (TFn as bs) (TFn [] ds) = pure $ TFn as (ds ++ bs)
composeFns (TFn as (b : bs)) (TFn (c : cs) ds) = do
  t <- mgu b c
  let as' = map (applySubst t) as
  let bs' = map (applySubst t) bs
  let cs' = map (applySubst t) cs
  let ds' = map (applySubst t) ds
  composeFns (TFn as' bs') (TFn cs' ds')
composeFns f@(TFn _ _) (TVar _) = pure f
composeFns (TVar _) f@(TFn _ _) = pure f
composeFns a b = throwError $ "Something went terribly wrong. Tried to compose " ++ show a ++ " with " ++ show b

composeFnsErr a b = case flip evalState 0 $ runExceptT $ composeFns a b of
  Left e -> error e
  Right r -> r

composeSubsts s1 s2 = M.map (applySubst s1) s2 `M.union` s1

mgu (TFn as bs) (TFn cs ds) = do
  stRest <- fresh
  l <- zipWithM mgu (as ++ [stRest]) (cs ++ [stRest])
  r <- zipWithM mgu (bs ++ [stRest]) (ds ++ [stRest])
  let l' = foldr composeSubsts M.empty l
  let r' = foldr composeSubsts M.empty r
  pure $ composeSubsts l' r'
mgu (TVar v) t = getSubst v t
mgu t (TVar v) = getSubst v t
mgu (TBase a) (TBase b)
  | a == b = pure M.empty
  | otherwise = throwError $ "Types do not unify: " ++ show a ++ " and " ++ show b
mgu a b = throwError $ "Types do not unify: " ++ show a ++ " and " ++ show b

getSubst v t
  | TVar v == t = pure M.empty
  | v `S.member` freevars t = throwError $ show v ++ " occurs in " ++ show t
  | otherwise = pure $ M.singleton v t

infer' env (AIntL _) = pure (M.empty, TFn [] [TBase TInt], env)
infer' env (ACall f) = do
  case M.lookup f env of
    Nothing -> throwError $ "infer': Unknown function: " ++ f
    Just t -> do
      tp <- instantiate t
      pure (M.empty, tp, env)
infer' env (ALamb l) = do
  -- _env' => wew. deklaracje w lambdzie
  (s, t, _env') <- infer env l
  -- let tp' = generalize (applySubst s env') t -- polimorficzne lambdy when??
  pure (s, TFn [] [t], env)
infer' _ (ADefn _ _) = assert False $ throwError "Invalid argument"
infer' _ (AComm _) = assert False $ throwError "Invalid argument"

infer env [] = do
  t <- fresh
  pure (M.empty, t, env)
infer env ((ADefn n body) : rest) = do
  (s, t, _env) <- infer env body
  guardE "s /= null" $ M.null s
  let env' = M.insert n (generalize env t) env
  infer env' rest
infer env ((AComm _) : rest) = infer env rest
infer env (a : rest) = do
  (sa, ta, env') <- infer' env a
  guardE "sa /= null" $ M.null sa
  (sr, tr, env'') <- infer (applySubst sa env') rest
  guardE "sr /= null" $ M.null sr
  tp <- composeFns ta tr
  pure (M.empty, tp, env'')

-- typeInference env e = runStateT $ runExceptT $ do
typeof env l = fst $ flip runState 0 $ runExceptT $ do
  (s, t, env') <- infer env l
  pure (applySubst s t, env')

tryMatchTypes a b = flip evalState 0 $ runExceptT $ aux
  where
    aux = do
      subst <- mgu a b
      trace (show (applySubst subst a)) $ pure ()
      trace (show (applySubst subst b)) $ pure ()
      pure (applySubst subst a)

promote :: (Ord s) => Tp s -> GeneralType s -- TODO
promote tp =
  if null fv
    then Mono tp
    else Poly $ Forall fv tp
  where
    fv = freevars tp

fresh :: ExceptT String (State Int) (Tp String)
fresh = do
  s <- get
  put (s + 1)
  pure $ TVar ("t" ++ show s)
