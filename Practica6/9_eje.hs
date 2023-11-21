{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
import Control.Applicative (Applicative (..))
import Control.Monad (liftM, ap)

newtype M m a = Mk ( m (Maybe a))

instance Monad m => Functor (M m) where
    fmap = liftM

instance Monad m => Applicative (M m) where
    pure a = Mk (return (Just a))
    (<*>) = ap


getM :: Monad m => M m a -> m (Maybe a)
getM (Mk m) = m


instance Monad m => Monad (M m) where
    m >>= f = Mk $ getM m >>= \case 
                Nothing -> return Nothing
                Just  a -> getM (f a)


{-
    Ley 1.
    return x >>= h = h x

    return x >>= h
    = {def return}
    Mk (return (just x)) >>= h
    = {def Bind, def getM}
    Mk (return (Just x) >>= \case -> ...)
    = {m monad}
    Mk (getM (h x))
    = {patt matching def getM}
    Mk mon
    where Mk mon = h x
    = {where}
    h x

    Ley 2.
    m >>= return = m

    m >>= return 
    = {def bind}
    Mk (mon >>= \case
            Nothing -> return Nothing)
            Just a  -> getM (f a)
    where Mk mon = m

    case Nothing
    = {def return}
    Mk (mon >>= return)
    where Mk mon = m
    = {mon monad}
    Mk mon 
    where Mk mon = m
    = {def where}
    m

    case Just a
    Mk (mon >>= \Just a -> getM return a)
    where Mk Mon = m
    = {def return}
    Mk (mon >>= \Just a -> getM (Mk (return (Just a))))
    where Mk mon = m
    = {def getM}
    Mk (mon >>= \Just a -> return Just a))
    where Mk mon = m
    = {b- reduction}
    Mk (mon >>= return)
    where Mk mon = m
    = {mon monad}
    Mk mon
    where Mk mon = m
    = {where}
    m

-}

throw :: Monad m => M m a
throw = Mk (return Nothing)

newtype StInt a = St {runSt :: Int -> (a,Int)}

instance Functor StInt where
    fmap = liftM

instance Applicative StInt where
    pure a = St (a,)  
    (<*>) = ap

instance Monad StInt where
    m >>= f = St (\n ->
                   let (a,n') = runSt m n
                   in runSt (f a) n')


type N a = M StInt a 

get:: N Int
get = Mk $ St $ \n -> (Just n,n)

put :: Int -> N ()
put n = Mk $ St $ const (Just (), n)

data Expr = Var
          | Con Int
          | Let Expr Expr
          | Add Expr Expr 
          | Div Expr Expr 
          deriving Show

eval :: Expr -> N Int
eval Var = get

eval (Con n) = return n

eval (Let e1 e2) =
    do v1 <- eval e1
       put v1
       eval e2

eval (Add e1 e2) = 
    do v1 <- eval e1
       v2 <- eval e2
       return (v1 + v2)

eval (Div e1 e2) =
    do v1 <- eval e1
       v2 <- eval e2
       if v2 == 0 then throw else return (v1 `div` v2)

runN :: N Int -> Int
runN (Mk f) = let (mb,_) = runSt f 0
              in case mb of
                 Nothing -> -1
                 Just n -> n