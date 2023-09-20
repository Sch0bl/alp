module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case  M.lookup v s of
                Just n  -> Right n
                Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update v n s = M.insert v n s

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm (Seq Skip c1) s   = (Right (c1 :!: s))
stepComm r@(Repeat c0 b) s = (Right (Seq c0 (IfThenElse b Skip r) :!: s))

stepComm (Let var ei) s =
  case evalExp ei s of
    Right (a :!: b)  -> Right (Skip :!: (update var a b))
    Left err -> Left err 

stepComm (IfThenElse b c0 c1) s = 
  case (evalExp b s) of
    Right (True :!: s')  -> Right (c0 :!: s')
    Right (False :!: s') -> Right (c1 :!: s')
    Left err -> Left err 

stepComm (Seq c0 c1) s = 
  case stepComm c0 s of
    Right (a :!: b) -> Right ((Seq a c1) :!: b)
    Left err -> Left err 

-- Evalua una expresion
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp BTrue      s = Right (True :!: s)
evalExp BFalse     s = Right (False :!: s)
evalExp (Const nv) s = Right (nv :!: s)
evalExp (Var x)    s = 
  case lookfor x s of
      Right n  -> Right (n :!: s)
      Left err -> Left err

evalExp (EAssgn v ei) s = 
  case (evalExp ei s) of
    Right (a :!: b) -> Right (a :!: update v a s)
    Left err -> Left err

evalExp cons s =
  let f op e e' s = 
        case (evalExp e s) of
          Right (a1 :!: b1) ->  
            case (evalExp e' b1) of
              Right (a2 :!: b2) -> Right (op a1 a2 :!: b2)
              Left err -> Left err 
          Left err -> Left err 
      g op e s = 
        case (evalExp e s) of
          Right (a :!: b) -> Right (op a :!: b)
          Left err -> Left err 
      h e e' s =
        case (evalExp e s) of
          Right (a1 :!: b1) -> 
            case (evalExp e' s) of
              Right (0 :!: b2)  -> Left DivByZero 
              Right (a2 :!: b2) -> Right (div a1 a2 :!: b2)
              Left err -> Left err
          Left err -> Left err
  in case cons of 
      (Plus e0 e1)  -> f (+) e0 e1 s
      (Minus e0 e1) -> f (-) e0 e1 s
      (Times e0 e1) -> f (*) e0 e1 s
      (Div e0 e1)   -> h e0 e1 s
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
