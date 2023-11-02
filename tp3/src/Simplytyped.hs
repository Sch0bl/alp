module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                        hiding  ( (>>=) ) 
import           Text.PrettyPrint.HughesPJ             ( render )
import           PrettyPrinter
import           Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n       ) = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b LUnit           = Unit
conversion' b (LPair lt lt' ) = Pair (conversion' b lt) (conversion' b lt')
conversion' b (LFst lt      ) = Fst (conversion' b lt)
conversion' b (LSnd lt      ) = Snd (conversion' b lt)
conversion' b (LApp t u     ) = conversion' b t :@: conversion' b u
conversion' b (LAbs n t u   ) = Lam t (conversion' (n : b) u)
conversion' b (LLet n t u   ) = Let (conversion' b t) (conversion' (n:b) u)
conversion' b LZero           = Zero
conversion' b (LSuc t       ) = Suc (conversion' b t)
conversion' b (LRec t1 t2 t3) = Rec (conversion' b t1) (conversion' b t2) (conversion' b t3)

-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) |   i == j  = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _  Unit                 = Unit 
sub _ t  Zero                 = Zero
sub _ _ (Free n             ) = Free n
sub i t (Pair t' u          ) = Pair ( sub i t t' ) (sub i t u)
sub i t (Fst u              ) = Fst $ sub i t u
sub i t (Snd u              ) = Snd $ sub i t u
sub i t (u       :@:      v ) = sub i t u :@: sub i t v
sub i t (Lam t' u           ) = Lam t' (sub (i + 1) t u)
sub i t (Let t' u           ) = Let (sub i t t') (sub i t u) 
sub i t (Suc t'             ) = Suc (sub i t t')
sub i t (Rec t1 t2 t3       ) = Rec (sub i t t1) (sub i t t2) (sub i t t3)

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _             ) = error "variable ligada inesperada en eval"
eval e (Free  n             ) = fst $ fromJust $ lookup n e
eval e Unit                   = VUnit
eval e (Pair t t'           ) = let v = eval e t in VPair v $ eval e t'
eval e (Fst t               ) = let VPair v _ = eval e t in v
eval e (Snd t               ) = let VPair _ v = eval e t in v
eval _ (Lam      t   u      ) = VLam t u
eval e (Lam _ u  :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u1 :@: u2     ) = let v2 = eval e u2 in eval e (sub 0 (quote v2) u1)

eval e (u        :@:      v ) = 
  case eval e u of
    VLam t u' -> eval e (Lam t u' :@: v)
    _         -> error "Error de tipo en run-time, verificar type checker"

eval e (Let t u             ) = eval e (sub 0 t u)
eval e Zero                   = VNum NZero
eval e (Suc t               ) = let VNum n = eval e t in VNum (NSuc n)
eval e (Rec t1 t2 Zero      ) = eval e t1
eval e (Rec t1 t2 (Suc t)   ) = eval e ((t2 :@:(Rec t1 t2 t)) :@: t)
eval e (Rec t1 t2 t3        ) = let vn = eval e t3 in eval e (Rec t1 t2 (quote vn)) 

-----------
--- quoting
-----------

quote :: Value -> Term
quote (VLam t f     ) = Lam t f
quote VUnit           = Unit
quote (VPair v v'   ) = Pair (quote v) (quote v')
quote (VNum NZero   ) = Zero
quote (VNum (NSuc n)) = Suc (quote (VNum n))

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

pairError :: Type -> Either String Type
pairError t  =
  err
    $  "se esperaba PairT, pero "
    ++ render (printType t)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i     ) = ret (c !! i)
infer' _ _ Unit           = Right UnitT
infer' c e Zero           = Right NatT
infer' c e (Lam t u     ) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let t t'    ) = infer' c e t >>= \tt -> infer' (tt : c) e t' >>= \tt' -> ret tt'
infer' c e (Pair t t'   ) = infer' c e t >>= \x -> infer' c e t' >>= \y -> ret $ PairT x y

infer' _ e (Free  n     ) =
  case lookup n e of
    Nothing     -> notfoundError n
    Just (_, t) -> ret t

infer' c e (t :@: u     ) =
  infer' c e t >>= \tt -> infer' c e u >>= \tu ->
    case tt of
      FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
      _          -> notfunError tt

infer' c e (Fst t       ) =
  infer' c e t >>= \t -> 
    case t of
      (PairT x _) -> ret x
      _           -> pairError t

infer' c e (Snd t       ) =
  infer' c e t >>= \t -> 
    case t of
      (PairT _ x) -> ret x
      _           -> pairError t

infer' c e (Suc t       ) = 
  infer' c e t >>= \t -> 
    if t /= NatT then matchError NatT t else ret t

infer' c e (Rec t1 t2 t3) =
  infer' c e t1 >>= \t -> infer' c e t2 >>= \t' -> infer' c e t3 >>= \t'' ->
     if t'' /= NatT then matchError NatT t'' else
      case t' of
        FunT a (FunT NatT a') ->
          if a == a' && a == t then ret a else err "mismatch Rec types" 
        _  -> err "mismatch Rec types"
----------------------------------