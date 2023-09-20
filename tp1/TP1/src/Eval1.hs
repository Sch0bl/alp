module Eval1
   ( eval
   , State
   )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple
import           Data.Maybe                    (fromJust) --added

-- Estados
type State = M.Map Variable Int

-- Estado nulo
initState :: State
initState = M.empty  

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Int
lookfor v s = fromJust (M.lookup v s) 

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update v n s = M.insert v n s  

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Pair Comm State
stepComm (Seq Skip c1) s   = (c1 :!: s)
stepComm r@(Repeat c0 b) s = (Seq c0 (IfThenElse b Skip r) :!: s)

stepComm (Let var ei) s = 
  let (a :!: b)  =  (evalExp ei s)
  in  (Skip :!: (update var a b))

stepComm (IfThenElse b c0 c1) s = 
  case (evalExp b s) of
    (True :!: s')  -> (c0 :!: s')
    (False :!: s') -> (c1 :!: s')

stepComm (Seq c0 c1) s =  
  let (a :!: b)  =  (stepComm c0 s)
  in (Seq a c1 :!: b)


-- Evalua una expresion
evalExp :: Exp a -> State -> Pair a State
evalExp BTrue      s = (True :!: s)
evalExp BFalse     s = (False :!: s)
evalExp (Const nv) s = (nv :!: s)
evalExp (Var x)    s = (lookfor x s :!: s)

evalExp (EAssgn v ei) s = 
  let (a :!: b) = (evalExp ei s)
  in  (a :!: update v a s)

evalExp cons s =
  let f op e e' s = 
        let (a1 :!: b1) = (evalExp e s)
            (a2 :!: b2) = (evalExp e' b1)
        in (op a1 a2 :!: b2)
      g op e s = 
        let (a :!: b) = (evalExp e s)
        in  (op a :!: b)
  in case cons of 
      (Plus e0 e1)  -> f (+) e0 e1 s
      (Minus e0 e1) -> f (-) e0 e1 s
      (Times e0 e1) -> f (*) e0 e1 s
      (Div e0 e1)   -> f (div) e0 e1 s
      (ESeq e0 e1)  -> f (\ x y -> y) e0 e1 s
      (UMinus e)    -> g (\x -> -x) e s
      ----------------
      (Eq e0 e1)    -> f (==) e0 e1 s
      (NEq e0 e1)   -> f (/=) e0 e1 s
      (Lt e0 e1)    -> f (<) e0 e1 s
      (Gt e0 e1)    -> f (>) e0 e1 s
      (Or e0 e1)    -> f (||) e0 e1 s
      (And e0 e1)   -> f (&&) e0 e1 s
      (Not e)       -> g (not) e s
