{-
Functores 
    fmap id = id
    fmap (f . g) = (fmap f) . (fmap g)
-}
--a)

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


