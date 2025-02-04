{-# LANGUAGE BangPatterns #-}

module Builtin where

import AST
import Ctx
import CtxTypes
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import Eval (eval')
import Inference
import InferenceTypes

stdenv = M.fromList $ map (\(n, t, _f) -> (n, t)) builtins

builtins :: [(Ident, Scheme Ident, Ctx -> Ctx)]
builtins =
  ("-", Forall S.empty (TFn [TBase TInt, TBase TInt] [TBase TInt]), ctxify minus)
    : ("+", Forall S.empty (TFn [TBase TInt, TBase TInt] [TBase TInt]), ctxify plus)
    : ("*", Forall S.empty (TFn [TBase TInt, TBase TInt] [TBase TInt]), ctxify mult)
    : ("/", Forall S.empty (TFn [TBase TInt, TBase TInt] [TBase TInt]), ctxify Builtin.div)
    : ("%", Forall S.empty (TFn [TBase TInt, TBase TInt] [TBase TInt]), ctxify Builtin.rem)
    : ("i", Forall (S.fromList ["a", "b"]) (TFn [TFn [TVar "a"] [TVar "b"], TVar "a"] [TVar "b"]), i)
    : ("[]", Forall S.empty (TFn [] [TFn [TVar "a"] [TVar "a"]]), ctxify lamid)
    : ("dup", Forall (S.fromList ["a", "b"]) (TFn [TVar "a"] [TVar "a", TVar "a"]), ctxify dup)
    : map
      aux -- FIXME
      [ ("swap", ("a b", "b a"), ctxify swap),
        ("zap", ("a b", "b"), ctxify zap),
        -- ("i", ("a", "b"), i),
        ("dip", ("a b", "b"), dip),
        -- ("cat", ("a b", "c"), ctxify cat),
        ("cons", ("a b", "c"), ctxify cons)
        -- ("[]", ("", ""), ctxify lamid)
      ]
  where
    aux (name, (stb, sta), f) = (name, Forall binds tp, f)
      where
        binds = S.fromList $ sta' ++ stb'
        stb' = words stb
        sta' = words sta
        tp = TFn (map TVar stb') (map TVar sta')

plus :: Stack -> Stack
plus (VInt a : VInt b : s) = VInt (a + b) : s
plus s = trace "minus: Stack underflow or invalid type" s

minus :: Stack -> Stack
minus (VInt a : VInt b : s) = VInt (a - b) : s
minus s = trace "minus: Stack underflow or invalid type" s

mult :: Stack -> Stack
mult (VInt a : VInt b : s) = VInt (a * b) : s
mult s = trace "mult: Stack underflow or invalid type" s

div :: Stack -> Stack
div (VInt a : VInt b : s) = VInt (a `Prelude.div` b) : s
div s = trace "div: Stack underflow or invalid type" s

rem :: Stack -> Stack
rem (VInt a : VInt b : s) = VInt (a `Prelude.rem` b) : s

lamid :: Stack -> Stack
lamid s = VFun (Interp [] (TFn [] []) M.empty M.empty) : s

ctxify :: (Stack -> Stack) -> Ctx -> Ctx
ctxify f ctx = replaceStack (f (getStack ctx)) ctx

swap :: Stack -> Stack
swap (a : b : s) = b : a : s
swap !s = trace ("swap: Stack underflow" ++ show s) s

dup :: Stack -> Stack
dup (a : s) = a : a : s
dup !s = trace ("dup: Stack underflow" ++ show s) s

zap :: Stack -> Stack
zap (_ : s) = s
zap !s = trace ("zap: Stack underflow" ++ show s) s

-- cat :: Stack -> Stack
-- cat (VFun a : VFun b : s) = f : s
--   where
--     f = VFun (Interp [ACall "0", ACall "1"] env)
--     -- ugly hack
--     env = M.insert "0" (a) $ M.insert "1" (b) M.empty
-- cat !s = trace ("cat: Stack underflow or invalid type") s

cons :: Stack -> Stack
cons (va : vb : s) = f : s
  where
    f = VFun (Interp [ALamb [ACall "1"], ACall "0"] t de te)
    t = composeFnsErr (tpOf a) (tpOf b)
    te = M.insert "1" (Forall S.empty (tpOf b)) $ M.insert "0" (Forall S.empty (tpOf a)) $ M.union eb ea
    a = asFn va
    b = asFn vb
    ea = tenv a
    eb = tenv b
    tenv (Interp _ _ _ te) = te
    tenv (Builtin _ _) = M.empty
    tpOf ((Interp _ tp _ _)) = tp
    tpOf ((Builtin _ (Mono tp))) = tp
    -- FIXME
    tpOf ((Builtin _ (Poly (Forall _ tp)))) = tp
    -- ugly hack
    asFn (VFun x) = x
    asFn (VInt i) = Interp [AIntL i] (TFn [] [TBase TInt]) M.empty M.empty
    de = M.insert "1" b $ M.insert "0" a $ M.union db da
    da = denv a
    db = denv b
    denv (Interp _ _ de _) = de
    denv (Builtin _ _) = M.empty
cons !s = trace ("cons: Stack underflow or invalid type") s

dip :: Ctx -> Ctx
dip ctx =
  let st = getStack ctx
   in case st of
        [] -> trace ("dip: Stack underflow") ctx
        [_] -> trace ("dip: Stack underflow") ctx
        (anyf@(VFun _) : b : s) -> push b $ i (replaceStack (anyf : s) ctx)
        (VInt x : _) -> trace ("dip: not a function: " ++ show x) ctx

i :: Ctx -> Ctx
i ctx =
  let st = getStack ctx
   in case st of
        [] -> trace ("i: Stack underflow") ctx
        (VFun (Builtin f tp) : s) -> f (replaceStack s ctx)
        (VFun dyn@(Interp {}) : s) -> case ev dyn (replaceStack s ctx) of
          Left e -> trace ("i: " ++ e) ctx
          Right ctx' -> ctx'
        (VInt x : _) -> trace ("i: not a function: " ++ show x) ctx
  where
    ev (Interp is _tp denv tenv) ct = do
      ct' <- eval' is $ replaceTEnv tenv (replaceDEnv denv ct)
      let st' = getStack ct'
      pure $ replaceStack st' ct
    ev _ _ = Left "i: not a function"
