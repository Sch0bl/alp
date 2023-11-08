{-
Functores 
    fmap id = id
    fmap (f . g) = (fmap f) . (fmap g)
-}
--a)

----------------------------- Ejercicio 1 ------------------------------
data Pair a = P (a,a)

instance Functor Pair where
  --fmap :: (a -> b) -> Pair a -> Pair b
    fmap f (P (a,a')) = P ((f a),(f a'))

{-
fmap id = id ?

fmap id 
= { def fmap}
\P (a, a') -> P ((id a), (id a')) 
= {def id}
\P (a, a') -> P (a, a') 
= {def id}
id

(fmap f) . (fmap g)
= { curry }
\P (a, a') -> (fmap f) . (fmap g) (P (a, a'))
= { def .}
\P (a, a') -> (fmap f ((fmap g (P (a, a')))
= { def fmap }
\P (a, a') -> (fmap f (P ((g a), (g a'))))
= { def fmap }
\P (a, a') -> P (f (g a), f (g a'))
= { def .}
\P (a, a') -> P (((f . g) a), ((f . g) a')) 
= {def fmap}
fmap (f . g)

-}

--b)

data Tree a = Empty | Branch a (Tree a) (Tree a)

instance Functor Tree where
  --fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Empty          = Empty

    fmap f (Branch a l r) = 
      let nl = fmap f l
          nr = fmap f r
      in  Branch (f a) nl nr


{-
fmap id = id ?

Probaremos por induccion estrcutural sobre Tree a

Caso t === Empty (*)
fmap id  
= { curry }
\t -> fmap id t
= {def fmap, (*)}
\t -> t
= {def id}
id

Suponemos que vale para los sub arboles de t
Caso t === Branch a l r (**)

fmap id
= { curry }
\t -> fmap id t
= { ** }
\(Branch a l r) -> fmap id (Branch a l r)
= {def fmap}
\(Branch a l r) -> (Branch (id a) (fmap id l) (fmap id r))
= {HI en l y r}
\(Branch a l r) -> (Branch (id a) l r)
= {def id}
\(Branch a l r) -> (Branch a l r)
= {def id}

finalmente de * y **
tenemos fmap id = id

fmap (f . g) = (fmap f) . (fmap g) ?

Probaremos por induccion estructural en Tree

Caso t === Empty (*)

Por un lado tenemos

fmap (f . g) Empty 
= {def famp}
Empty

por otro

(fmap f) . (fmap g) Empty
= {def .}
fmap f (fmap g Emty)
= {def fmap}
fmap f Empty
= {def fmap}
Empty

luego
fmap (f . g) = (fmap f) . (fmap g) 

Suponemos que la igualdad vale para sub-arboles de t

Caso t === (Branch a l r)

fmap (f . g) (Branch a l r)
= {def fmap}
(Branch ((f . g) a) (fmap (f . g) l) (fmap (f . g) r))
= {HI}
(Branch ((f . g) a) ((fmap f) . (fmap g) l) ((fmap f) . (fmap g) r))
= {def .}
Branch ((f (g a)) ((fmap f (fmap g l)) ((fmap f (fmap g r))
= {def fmap}
fmap f (Branch (g a) (fmap g l) (fmap g r))
= {def fmap}
fmap f (fmap g (Branch a l r))
= {def .}
(fmap f) . (fmap g) (Branch a l r)

luego de los dos casos tenemos que la igualdad vale

-}

data GenTree a = Gen a [GenTree a]

instance Functor GenTree where
  fmap f (Gen a xs) = Gen (f a) $ map (fmap f) xs

{-
fmap id = id ?

Notemos que map es fmap de listas y por lo tanto vale que 
-- map id = id                                     (1)
-- map (f . g) = (map f) . (map g)                 (2)

Probamos por inducción estructural en GenTree

Suponemos que la igualdad vale para todos los GenTree en xs

fmap id (Gen a xs)
= {def fmap}
Gen (id a) $ map (fmap id) xs
= {HI}
Gen (id a) $ map id xs
= {def id y 1}
Gen a xs

Lugo por extensionalidad tenemos fmap id = id

ahora 

fmap (f . g) = (fmap f) . (fmap g)?

suponemos que la igualdad vale para todo GenTree en xs

fmap (f . g) (Gen a xs)
= {def fmap}
Gen ((f . g) a) $ map (fmap (f . g)) xs
= {HI}
Gen ((f . g) a) $ map ((fmap f) . (fmap g)) xs
= {def . y (2)}
Gen (f (g a)) $ map (fmap f) (map (fmap g) xs)
= {def fmap}
fmap f (Gen (g a) $ map (fmap g) xs)
= {def fmap}
fmap f (fmap g (Gen a xs))
= {def .}
(fmap f) . (fmap g) (Gen a xs)

Luego, por extensionalidad tenemos que la igualdad vale .
-}

data Cont a = C ((a -> Int) -> Int)
--Ejemplos

ejje :: (Int -> Int) -> Int
ejje f = f 3

eje :: Cont Int 
eje = C ejje 

instance Functor Cont where
  fmap f (C h) = C $ \app -> h (app . f)   

{-
fmap id = id ?

fmap id (C h)
= {def fmap}
C $ \app -> h (app . id)
= {def id}
C $ \app -> h app
= {etha reduction}
C h

Luego por extensionalidad tenemos fmap id = id

fmap (f . g) = (fmap f) . (fmap g)?

(fmap f) . (fmap g) (C h)
= {def .}
fmap f (fmap g (C h))
= {def fmap}
fmap f (C $ \app -> h (app . g))
= {def fmap}
C $ \app' -> (\app -> h (app . g)) (app' . f))
= {b reduction}
C $ \app' -> h ((app' . f) . g)
= {def . (monoide)}
C $ \app' -> h (app' . (f . g))
= {def fmap}
fmap (f . g) (C h)
-}


----------------------------- Ejercicio 2 ------------------------------


data Func a = F (a -> a)

instance Functor Func where
  fmap g (F f) = F id

{-

Observermos que la propiedad fmap id = id no se cumple
ya que si tomamos f != id tenemos por un lado 
fmap id (F f) = (F id)
pero por el otro tenemos
id (F f) = (F f)
donde (F f) != (F id)
por lo cual fmap id != id

-}

data Br b a = B b (a,a)

instance Functor (Br b) where 
  fmap f (B x (y,z)) = B x (f z, f y)

{-
Analogo al anterior, tomando z != y
-}


----------------------------- Ejercicio 3 ------------------------------

-- a)
data MyEither e a = ML e | MR a 

instance Functor (MyEither e) where
  fmap f (ML e) = (ML e)
  fmap f (MR a) = (MR (f a))

instance Applicative (MyEither e) where
  -- pure :: (a -> b) -> MyEither (a -> b)
  pure = MR
  -- <*> :: MyEither (a -> b) -> MyEither a -> MyEither b
  (ML   e) <*>    _    = ML e  
  (MR  f ) <*> (ML  e) = ML e 
  (MR  f ) <*> (MR  a) = MR  (f a)

-- b)
{-
instance Applicative ((->) r) where
  pure :: a -> (r -> a)
  pure a = ((->) r a)
  <*> :: (a -> b) -> (r -> a) -> (r -> b)
  h <*> f = h . f
-}

----------------------------- Ejercicio 4 ------------------------------

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 op h i = (pure op) <*> h <*> i

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> k)
                           ->
                           f a ->
                           f b ->
                           f c ->
                           f d ->
                           f e ->
                           f k 
liftA5 op fa fb fc fd fe = 
  (pure op) <*> fa <*> fb <*> fc <*> fd <*> fe 

----------------------------- Ejercicio 5 ------------------------------

sA :: Applicative f => [f a] -> f [a]
sA [] = pure []
sA (x:xs) = let fl = sA xs
                   in (pure (:)) <*> x <*> fl 

----------------------------- Ejercicio 6 ------------------------------
{-
Probar que todo monada es un functor, es decir proveer una instancia

instance Monad m => Functor m where
    fmap :: (a -> b) -> m a -> m b
    fmap f = \x -> x >>= \a -> return (f a)

fmap id 
= {def fmap}
\x -> x >>= \a -> return (id a)
= {def id}
\x -> x >>= \a -> return a
= {etha redex}
\x -> x >>= return
= {def monad.2}
\x -> x
= {def id}
id

Therefore fmap id x = x

fmap f . fmap g
= {def .}
\y -> fmap f (fmap g y)
= {def fmap}
\y -> fmap f (y >>= \a -> return (g a))
= {def fmap}
\y -> y >>= \a -> return (g a) >>= \b -> return (f b)
= {def monad 3 and return}
\y -> y >>= return >>= \a -> return (f (g a))
= {def monad 2}
\y -> y >>= \a -> return (f (g a))
= {def .}
\y -> y >>= \a -> return ((f . g) a)
= {def fmap}
fmap f . g
-}