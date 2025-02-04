{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module InferenceTypes where

import qualified Data.Map as M
import qualified Data.Set as S

data BaseType = TInt deriving (Eq, Ord, Show)

data Tp v
  = TBase BaseType
  | TFn [Tp v] [Tp v]
  | TVar v
  deriving (Eq, Ord)

instance (Show v) => Show (Tp v) where
  show (TBase TInt) = "Int"
  show (TFn as bs) = "(" ++ show' as ++ " -- " ++ show' bs ++ ")"
    where
      show' = foldr (\x acc -> show x ++ (if null acc then "" else " ") ++ acc) ""
  show (TVar v) = show v

data Scheme v
  = Forall (S.Set v) (Tp v)
  deriving (Eq, Ord)

instance (Show v) => Show (Scheme v) where
  show (Forall binds tp) = "∀" ++ concatMap s (S.toList binds) ++ ". " ++ show tp
    where
      s v = show v ++ " "

type TEnv v = M.Map v (Scheme v)

type Subst v = M.Map v (Tp v)

data GeneralType s
  = Mono (Tp s)
  | Poly (Scheme s)
  deriving (Eq, Ord, Show)

-- data Constraint v
--   = (Tp v) :≡ (Tp v)
--   | (Tp v) :≤ (Tp v)
--   | (Tp v) :≼ (Scheme v)
