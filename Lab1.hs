-- Chenhan Ma 
-- 823289
-- Declarative Programming Assignment 1


module Lab1 (subst, interleave, unroll) where

--subst
subst :: Eq t => t -> t -> [t] -> [t]
subst a b [] = []
subst a b (x:xs) 
    | a==x      = b:(subst a b xs)
    | otherwise = x:(subst a b xs)

--interleave
interleave :: [t] -> [t] -> [t]
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) = x:y:(interleave xs ys)

--unroll
unroll :: Int -> [a] -> [a]
unroll n [] = []
unroll n xs = take n (cycle xs)