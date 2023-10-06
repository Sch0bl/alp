module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

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
vapp (VLam f) v = f v 
vapp (VNeutral n) v = VNeutral (NApp n v)  

{-
type NameEnv v = [(Name, v)] -- Nombre de entorno [(Name, Valor)]
-}
eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv)  = lEnv !! ii
eval' (Free s) (gEnv, lEnv) = case lookup s gEnv of
                                Just n -> n    
                                _ -> VNeutral (NFree s)
eval' (Lam l) (gEnv, lEnv)  = VLam (\x -> (eval' l (gEnv, x : lEnv))) 
eval' (t1 :@: t2) e  = vapp (eval' t1 e) (eval' t2 e)

                       --   case eval' t1 e of 
                       --   v@(VNeutral n) ->  vapp v (eval' t2 e)
                       --   (VLam v) -> v (eval' t2 e)   

--searchGEnv :: Name -> NameEnv -> Maybe Value 
--searchGEnv s [] = Nothing  
--searchGEnv s (x:xs) | s == fst x  = Just snd x
--                    | otherwise   =  searchGEnv s xs    
--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote v = quote' v 0 

quote' :: Value -> Int -> Term 
quote' (VNeutral v) i = auxN v i  
quote' (VLam f) i = let nv = f (VNeutral (NFree (Quote i))) 
                    in Lam $ quote' nv (i + 1)

auxN :: Neutral-> Int -> Term 
auxN (NFree (Global s)) i = Free (Global s)
auxN (NFree (Quote k)) i =  (Bound  (i - k - 1)) 
auxN (NApp n v) i = (auxN n i) :@: (quote' v i) 
