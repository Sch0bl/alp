module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple
import           Data.Maybe                    (fromJust)

-- Estados
type State = M.Map Variable Int

-- Estado nulo
-- Completar la definición
initState :: State
initState = M.empty  

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s = fromJust (M.lookup v s) 

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v n s = M.adjust (\x -> n) v s  

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm (Let var ei) s = let (a :!: b)  =  (evalExp ei s)
                          in  (Skip :!: (update var a b))
stepComm (Seq Skip c1) s = (c1 :!: s)
stepComm (Seq c0 c1) s =  let (a :!: b)  =  (stepComm c0 s)
                          in (Seq a c1 :!: b)
stepComm (IfThenElse b c0 c1) s = case (evalExp b s) of
                                    (True :!: s') ->  (c0 :!: s')
                                    (False :!: s') -> (c1 :!: s')
stepComm r@(Repeat c0 b) s = (Seq c0 (IfThenElse b Skip r) :!: s)



-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const nv) s = (nv :!: s)
evalExp (Var x) s = (lookfor x s :!: s)
evalExp (UMinus ei) s = let (a :!: b) = (evalExp ei s)
                        in (-a :!: b)
evalExp (EAssgn v ei) s = let (a :!: b) = (evalExp ei s)
                          in (a :!: update v a s)
--evalExp (Plus e0 e1) s =  let (a1 :!: b1) = (evalExp e0 s)
--                              (a2 :!: b2) = (evalExp e1 b1)
--                          in (a1 + a2 :!: b2) 
--evalExp (Minus e0 e1) s = let (a1 :!: b1) = (evalExp e0 s)
--                              (a2 :!: b2) = (evalExp e1 b1)
--                          in (a1 - a2 :!: b2)
--evalExp (Times e0 e1) s = let (a1 :!: b1) = (evalExp e0 s)
--                              (a2 :!: b2) = (evalExp e1 b1)
--                          in (a1 * a2 :!: b2)
--evalExp (Div e0 e1) s = let (a1 :!: b1) = (evalExp e0 s)
--                            (a2 :!: b2) = (evalExp e1 b1)
--                          in (div a1 a2 :!: b2)
--------------------------------------------
evalExp BTrue s = (True :!: s)
evalExp BFalse s = (False :!: s)
evalExp (Not ei) s =  let (a :!: b) = (evalExp ei s)
                      in (not a :!: b)
--evalExp (Eq e0 e1) s =  let (a1 :!: b1) = (evalExp e0 s)
--                            (a2 :!: b2) = (evalExp e1 b1)
--                        in (a1 == a2 :!: b2)
--evalExp (NEq e0 e1) s = let (a1 :!: b1) = (evalExp e0 s)
--                            (a2 :!: b2) = (evalExp e1 b1)
--                        in (a1 != a2 :!: b2) 
--evalExp (Lt e0 e1) s =  let (a1 :!: b1) = (evalExp e0 s)
--                            (a2 :!: b2) = (evalExp e1 b1)
--                        in (a1 < a2 :!: b2)
--evalExp (Gt e0 e1) s =  let (a1 :!: b1) = (evalExp e0 s)
--                            (a2 :!: b2) = (evalExp e1 b1)
--                        in (a1 > a2 :!: b2)
--evalExp (Not ei) s =  let (a :!: b) = (evalExp ei s)
--                      in (!a :!: b)
--evalExp (Or e0 e1) s =  let (a1 :!: b1) = (evalExp e0 s)
--                            (a2 :!: b2) = (evalExp e1 b1)
--                        in (a1 || a2 :!: b2)
--evalExp (And e0 e1) s = let (a1 :!: b1) = (evalExp e0 s)
--                            (a2 :!: b2) = (evalExp e1 b1)
--                        in (a1 && a2 :!: b2)
evalExp cons s =
  -- let f op e e' = let (a1 :!: b1) = (evalExp e0 s)
  --                     (a2 :!: b2) = (evalExp e1 b1)
  --                 in (op a1 a2 :!: b2)
  let
  -- f :: (a -> a -> c) -> (Exp a) -> (Exp a) -> State -> Pair c State
  f op e e' s = let (a1 :!: b1) = (evalExp e s)
                    (a2 :!: b2) = (evalExp e' b1)
                in (op a1 a2 :!: b2)
  in case cons of 
      (Plus e0 e1)  -> f (+) e0 e1 s
      (Minus e0 e1) -> f (-) e0 e1 s
      (Times e0 e1) -> f (*) e0 e1 s
      (Div e0 e1)   -> f (div) e0 e1 s
      (ESeq e0 e1)  -> f (\ x y -> y) e0 e1 s
      ----------------
      (Eq e0 e1)    -> f (==) e0 e1 s
      (NEq e0 e1)   -> f (/=) e0 e1 s
      (Lt e0 e1)    -> f (<) e0 e1 s
      (Gt e0 e1)    -> f (>) e0 e1 s
      (Or e0 e1)    -> f (||) e0 e1 s
      (And e0 e1)   -> f (&&) e0 e1 s
