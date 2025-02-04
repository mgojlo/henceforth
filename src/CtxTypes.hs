module CtxTypes where

import AST
import qualified Data.Map as M
import InferenceTypes

type Stack = [Value]

data Value
  = VInt Int
  | VFun Fun

instance Show Value where
  show (VInt i) = show i
  show (VFun f) = "λ" ++ show f

data Fun
  = Interp [AST] (Tp Ident) DEnv (TEnv Ident)
  | Builtin (Ctx -> Ctx) (GeneralType Ident)

instance Show Fun where
  show (Interp is _ _ _) = "\"" ++ foldr (\x acc -> if null acc then show x else show x ++ " " ++ acc) "" is ++ "\""
  show (Builtin _f _) = "<builtin>"

type DEnv = M.Map Ident Fun

type Ctx = (Stack, TEnv Ident, DEnv)
