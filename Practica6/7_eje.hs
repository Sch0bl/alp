import Control.Applicative (Applicative (..))
import Control.Monad (liftM, ap)

newtype Cont r a = C {runC :: (a -> r) -> r}

instance Functor (Cont r) where
    fmap = liftM

instance Applicative (Cont r) where
    pure a = C $ \f -> f a
    (<*>) = ap

instance Monad (Cont r) where
    m >>= f = C (\g -> runC m (\a -> runC (f a) g)) 

{-
    Ley 1. return x >>= j = j x

    return x >>= j
    = {def return}
    C $ \f -> f x >>= j
    = {def bind}
    C (\g -> (\f -> f x) (\a -> runC (j a) g))
    = {b - reduction}
    C (\g -> (\a -> runC (j a) g) x)
    = {b - reduction}
    C (\g -> runC (j x) g)
    = {where}
    C (\g -> runC (C k) g)
    where C k = j x
    = {def runC}
    C (\g -> k g)
    where C k = j x
    = {b-reduction}
    C k
    where C k = j x
    = {where}
    j x

    Ley 2. m >>= return = m
    
    C h >>= return
    = {def bind}
    C (\g -> h (\a -> runC (return a) g))
    = {def return}
    C (\g -> h (\a -> runC (\f -> f a) g))
    = {def runC}
    C (\g -> h (\a -> g a))
    = {b-reduction}
    C (\g -> h g)
    = {b-reduction}
    C h

    Ley 3. m >>= (\x -> k a >>= h) = (m >>= k) >>= h
    C h >>= (\x -> k a >>= i)
    = {def bind and runC}
    C (\g -> 
       h (\a ->
          runC (k a >>= i) g))
    = {bind}
    C (\g -> 
        h (\a ->
           runC (C (\g ->
                    runC (k a) (\b -> runC (i b) g))) g )
    = {def runC b - reduction}
    C (\g ->
        h (\a -> 
            runC (k a) (\b -> runC (i b) g)))
    = {b - expansion}
    C (\g ->
         (\g -> h (\a -> runC (k a) g)) (\b -> runC (i b) g))
    = {def Let}
    C (\g ->
        let C x = (C (\g -> h (\b -> runC (k b) g)))
        in x (\b -> runC (i b) g)
    = {def bind}
    C (\g ->
        let C x = (C h >>= k))
        in x (\b -> runC (i b) g))
    = {def bind}
    (C h >>= k) >>= i
-}