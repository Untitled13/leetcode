import Data.List

merge :: (a -> a -> Bool)  -> [a] -> [[a]]
merge _ []       = []
merge f [x]      = [[x]]
merge f (x:y:zs)   
     | f x y     = applyFirst (x:) $ merge f $ y:zs
     | otherwise = [x] : (merge f $ y:zs)
          where applyFirst :: (a -> a) -> [a] -> [a]
                applyFirst _ []     = []
                applyFirst f (x:xs) = f x : xs 

f :: Ord a => (a,a) -> (a,a) -> Bool
f = \(_,x) (y,_) -> x >= y

merge' :: Ord a => [(a,a)] -> [(a,a)]
merge' xs = let res  = merge f $ sort xs
                mins = map minimum $ map (map fst) res
                maxs = map maximum $ map (map snd) res
             in zip mins maxs