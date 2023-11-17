msort :: Ord a => [a] -> [a]
msort [a] = [a]  
msort ls = let (xs, ys) = divide ls 
           in merge (msort xs) (msort ys)

divide :: [a] -> ([a],[a])
divide []         = ([],[])
divide l@[x]   = (l,[])
divide (x:(y:zs)) =
    let (xs,ys) = divide zs
    in (x:xs,y:ys)
                
merge :: Ord a => [a] -> [a] -> [a]
merge     []        []      = []
merge     xs        []      = xs 
merge     []        xs      = xs 
merge xs@(x:xs') ys@(y:ys') =
    if x <= y
    then x : merge xs' ys
    else y : merge ys' xs


arion :: [a] -> [a]
arion [a,b,c,d] = [b,a,d,c]


isPrimo :: Int -> Bool
isPrimo 1 = True
isPrimo n = not $ isPrimo' n (n - 1)

isPrimo' :: Int -> Int -> Bool
isPrimo' p 1 = False
isPrimo' p n = mod p n == 0 || isPrimo' p (n - 1)