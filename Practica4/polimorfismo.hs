{-# LANGUAGE RankNTypes #-}
import Control.Monad (when)

--data Nat = Zero | Succ Nat deriving Show

newtype Nat = Nat {runNat :: forall a. (a -> a) -> a -> a}

data MyNat = Zero | Succ MyNat

nat :: Int -> Nat
nat 0 = zero
nat n = 
    let r = nat $ n - 1 
    in Nat $ \f z -> f $ runNat r f z

zero :: Nat
zero = Nat $ \f z -> z

suc :: Nat -> Nat
suc n = Nat $ \f z -> f $ runNat n f z

double:: forall a. (a -> a) -> a -> a
double f x = f $ f x

newtype MyBool = MyBool {runBool :: forall a . a -> a -> a}

t :: MyBool
t = MyBool const

f :: MyBool
f = MyBool $ \x y -> y

newtype Pair b c = P {runP :: forall a. (b -> c -> a) -> a}

pairP ::forall a b. a -> b -> Pair a b
pairP a b = P $ \op -> op a b

fstP ::forall a b. Pair a b -> a
fstP p = runP p const

sndP ::forall a b. Pair a b -> b
sndP p = runP p (\x y -> y)


newtype List a = List {runList :: forall r. (a -> r -> r) -> r -> r}

nil :: forall a. List a
nil = List $ \c n -> n

cons :: forall a. a -> List a -> List a
cons a l = List $ \ c as -> c a $ runList l c as

null :: forall a. List a -> MyBool
null l = MyBool $ \t f -> runList l (\c n -> f) t

-- Ejercicio 11

myAnd :: MyBool -> MyBool -> MyBool
myAnd b1 b2 = MyBool $ \x y -> runBool b1 (runBool b2 x y) y

-- Ejercicio 12

pairNat :: Nat -> Nat -> Pair Nat Nat
pairNat n1 n2 = P $ \f -> f n1 n2

firstNat :: Pair Nat Nat -> Nat
firstNat p = runP p const

secondNat :: Pair Nat Nat -> Nat
secondNat p = runP p (\x y -> y)

-- Ejercicio 13

pred' :: Nat -> Pair Nat Nat
pred' n = runNat n shiftAdd $ pairNat zero zero

shiftAdd :: Pair Nat Nat -> Pair Nat Nat
shiftAdd p = runP p (\n1 n2 -> pairNat (suc n1) n1)

predNat :: Nat -> Nat
predNat n = secondNat $ pred' n

-- Ejercicio 14

    -- a)
mapL :: forall a b. (a -> b) -> List a -> List b
mapL f l = List $ \c n -> runList l (c . f) n

appendL :: forall a. List a -> List a -> List a
appendL l1 = runList l1 cons

reverseL :: forall a. List a -> List a
reverseL l = runList l (\h tl -> appendL tl (cons h nil)) nil

    -- b)

sumN :: Nat -> Nat -> Nat
sumN n1 = runNat n1 suc

sumL :: List Nat -> Nat
sumL l = runList l sumN zero

    -- C)

singleton :: forall a . a -> List a
singleton a = cons a nil

headTail :: forall a . List a -> Pair (List a) (List a)
headTail l = runList l (\a r -> pairP (appendL (singleton a) (fstP r)) (sndP r)) (pairP nil nil)

tailL :: forall a. List a -> List a
tailL l = sndP $ headTail l

headL :: forall a. List a -> a -> a
headL l = runList l const

insert :: forall a. (a -> a -> MyBool) -> List a -> a -> List a
insert cmp l a =
    runList l
    (\b r ->
       runBool (cmp b (headL r b))
       (cons b r)
       (cons (headL r b) (cons b (tailL r))))
    (singleton a)