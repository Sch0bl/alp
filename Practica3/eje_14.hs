-- Teste para el ejercicio 14
--
--
---------------------------------

data Nat = Zero | Succ Nat deriving Show

cuatro = numNat 4
cinco = numNat 5
seis = numNat 6
diez = numNat 10

eval f m n = showNat(myToNat (f (natToMy (numNat m)) (natToMy (numNat n))))

myCuatro = mySucc (mySucc (mySucc (mySucc myZero)))

myDos = mySucc (mySucc myZero)

myZero = \f z -> z

mySucc = \n f z -> f (n f z)

myFoldn = (\x -> x)

myToNat = (\n -> n Succ Zero)

natToMy n = foldn n (\n' -> (\f -> \z -> f (n' f z))) (\f -> \z -> z)

mySuma m n = m mySucc n

myProd n m = m (mySuma n) myZero

myPot n m = m (myProd n) (mySucc myZero)

foldn :: Nat -> (a -> a) -> a -> a
foldn Zero f z = z
foldn (Succ n) f z = f (foldn n f z)

numNat :: Int -> Nat
numNat 0 = Zero
numNat n = Succ (numNat (n - 1))

isZero :: Nat -> Bool
isZero n = foldn n (\n -> False) True 

showNat :: Nat -> Int
showNat n = foldn n (1+) 0 

sumaNat :: Nat -> Nat -> Nat  
sumaNat n m = foldn m Succ n

predNat' :: Nat -> (Nat, Nat)
predNat' n = foldn n (\(f,s) -> (Succ f, f)) (Zero,Zero) 

predNat :: Nat -> Nat
predNat n = s
        where
              (_,s) = predNat' n

maxNat :: Nat -> Nat -> Nat
maxNat m n = if isZero (subNat m n) then n else m

subNat :: Nat -> Nat -> Nat
subNat m n = foldn n predNat m

data BinTree a = Leaf | Bin a (BinTree a) (BinTree a) deriving Show

dataTree = (Bin 9
           (Bin 4 (Bin 0 (Bin 0 Leaf Leaf) Leaf) (Bin 1 Leaf Leaf))
           (Bin 6 (Bin 15 Leaf Leaf) (Bin 10 Leaf Leaf)))

foldBin :: BinTree a -> b -> (a -> b -> b -> b) -> b
foldBin Leaf l b = l
foldBin (Bin a t u ) l b = b a (foldBin t l b) (foldBin u l b)

isLeaf :: BinTree a -> Bool
isLeaf d = foldBin d True (\a -> \l -> \r -> False)

mapBin :: (a -> b) -> BinTree a -> BinTree b
mapBin f d = foldBin d Leaf (\a -> \l -> \r -> (Bin (f a) l r))

heightBin :: BinTree a -> Nat
heightBin d = foldBin d Zero (\a -> \l -> \r -> (Succ (maxNat l r)))

mirrorBin :: BinTree a -> BinTree a
mirrorBin d = foldBin d Leaf (\a -> \l -> \r -> (Bin a r l))
