msort :: Ord a => [a] -> [a]
msort [a] = [a]  
msort ls = let (xs, ys) = divide ls 
           in merge (msort xs) (msort ys)

divide :: [a] -> ([a],[a])
divide []         = ([],[])
divide l@(x:[])   = (l,[])
divide (x:(y:zs)) =
    let (xs,ys) = divide zs
    in (x:xs,y:ys)
                
merge :: Ord a => [a] -> [a] -> [a]
merge     []        []      = []
merge     xs        []      = xs 
merge     []        xs      = xs 
merge xs@(x:xs') ys@(y:ys') =
    if x <= y
    then (x:merge xs' ys)
    else (y:merge ys' xs)