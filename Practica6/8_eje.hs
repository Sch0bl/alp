import Control.Applicative (Applicative (..))
import Control.Monad (liftM, ap)

type Error = Either

data T = Con Int | Div T T

newtype M s e a = M {runM :: s -> Error e (a,s)}

instance Functor (M s e) where
    fmap = liftM

instance Applicative (M s e) where
    pure a = M (\s -> Right (a, s))
    (<*>) = ap

instance Monad (M s e) where
    m >>= f =
        M (\s ->
            case runM m s of
               Left e -> Left e
               Right (a,s') -> runM (f a) s')

raise :: e -> M s e a 
raise e = M $ const $ Left e 

modify :: (s -> s) -> M s e () 
modify f = M (\s -> Right ((), f s)) 

eval :: T -> M Int String Int
eval (Con n) = return n
eval (Div t1 t2) = 
    do v1 <- eval t1
       v2 <- eval t2
       if v2 == 0 
       then raise "Error: Division por cero."
       else do modify (+1)
               return (v1 `div` v2)

evalC :: T -> M Int String Int
evalC (Con n) = return n
evalC (Div t1 t2) = 
    evalC t1 >>= 
    \v1 -> evalC t2 >>= 
    \v2 -> if v2 == 0
           then raise "Error"
           else modify (+1) >> return (v1 `div` v2)

evalCu :: T -> M Int String Int
evalCu (Con n) = M (\s -> Right (n,s))
evalCu (Div t1 t2) = 
    M (\s ->
        case runM (evalCu t1) s of
            Left e -> Left e
            Right (v1, s') ->
                case runM (evalCu t2) s' of
                    Left e -> Left e
                    Right (v2, s'') ->
                        if v2 == 0 
                        then Left "Error"
                        else Right (v1 `div` v2, (+1) s'')) 