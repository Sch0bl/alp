import Control.Applicative (Applicative (..))
import Control.Monad (liftM, ap)
{-# LANGUAGE TupleSections #-}

newtype State s a = St {runSt :: s -> (a,s)}

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure a = St (a,)
    (<*>) = ap

instance Monad (State s) where
    (St h) >>= f = 
        St (\s ->
            let (x,s') = h s
            in runSt (f x) s')

set :: s -> State s ()
set s = St $ const ((),s)

get :: State s s
get = St $ \s -> (s,s) 

{-
    Ley 1. return x >>= j = j x

    return x >>= j
    = {def return}
    St (x,) >>= j
    = {def bind}
    St (\s ->
        let (x',s') = (x,s)
        in runSt (j x) s')
    = {def Let}
    St (\s -> runSt (j x) s)
    = {pattern mathcing}
    St (\s -> runSt (St g) s)
    where St g = j x
    = {def runState}
    St (\s -> g s)
    where St g = jx
    = { b-reduction }
    St g
    where St g = jx
    = {where}
    j x

    Ley 2. m >>= return = m
    
    St h >>= return
    = {def bind}
    St (\s ->
        let (x,s') = h s
        in runSt (return x) s')
    = {def return}
    St (\s ->
        let (x,s') = h s
        in runSt (St $ (x,)) s')
    = {def runSt}
    St (\s ->
        let (x,s') = h s
        in (x,s')) 
    = {def Let}
    St (\s -> h s)
    = {b - reduction}
    St h

    Ley 3. m >>= (\x -> k a >>= h) = (m >>= k) >>= h
    St f >>= (\x -> k a >>= h)
    = {def bind}
    St (\s -> 
        let (x,s') = f s
        in runSt (St g >>= h) s')
    where St g = k x
    = {def bind}
    St (\s -> 
        let (x,s') = f s
        in runSt (St (\s ->
                      let (x',s'') = g s
                      in runSt (h x') s'')) s')
    where St g = k x
    = {def runSt}
    St (\s -> 
        let (x,s') = f s
        in let (x',s'') = g s'
           in runSt (h x') s'')
    where St g = k x
    = {where}
    St (\s -> 
        let (x,s') = f s
        in let (x',s'') = g s'
           in j s'')
    where St g = k x
          St j = h x'
    = {sem let}
    St (\s ->
        let (x',s'') = let (x,s') = f s
                       in g s'
        in j s'')
    where St g = k x
          St j = h x'
    = {def runSt}
    St (\s ->
        let (x',s'') = let (x,s') = f s
                       in runSt (St g) s'
        in runSt (St j) s'')
    where St g = k x
          St j = h x'
    = {where}
    St (\s ->
        let (x',s'') = let (x,s') = f s
                       in runSt (k x) s'
        in runSt (h x) s'')
    = {def runST}
    St (\s ->
        let (x,s') = runSt (\s ->
                            let (x',s'') = f s
                            in runSt (k x') s'') s
        in runSt (h x) s')
    = {def bind}
    St (\s ->
        let (x,s') = runSt (St f >>= k) s
        in runSt (h x) s')
    = {def bind, pattern}
    (St f >>= k) >>= h
-}