module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------
{-data Term  = Bound Int
            | Free Name
            | Term :@: Term
            | Lam Term
        deriving (Show,Eq)

-- Tipos de los nombres
data Name
    =  Global  String
    |  Quote   Int
  deriving (Show, Eq)

-- Términos con nombres
data LamTerm  =  LVar String
              |  App LamTerm LamTerm
              |  Abs String LamTerm
              deriving (Show, Eq)

-}

conversion :: LamTerm -> Term
conversion t = conv' [] t 

conv' :: [String] -> LamTerm -> Term 
conv' l (LVar v) = case (findT l 0 v) of 
                    Just n -> Bound n 
                    Nothing -> Free (Global v) 
conv' l (Abs s t) = Lam (conv' (s:l) t) 
conv' l (App t1 t2) = termRC l t1 :@: termRC l t2  

termRC :: [String] -> LamTerm -> Term 
termRC l t = case t of 
                Abs _ _ -> conversion t 
                otherwise -> conv' l t    

findT :: [String] -> Int -> String -> Maybe Int 
findT [] _ _  = Nothing 
findT (x:xs) i s    | s == x = Just i
                    | otherwise = (findT xs (i + 1) s) 

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f) (VNeutral t) = f t 
vapp (VLam f) (VLam t) = (VLam f . t) 
vapp (VNeutral n) (VNeutral t) = undefined 
vapp (VNeutral n) (VLam t) = undefined


eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' _          _         = undefined


--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = undefined






