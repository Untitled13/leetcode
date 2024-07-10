import Control.Applicative

-- rm :: [a] -> [[a]]
-- rm []     = []
-- rm (x:xs) = (xs:) $ (x:) <$> rm xs

-- permutations :: [a] -> [[a]]
-- permutations [] = [[]]
-- permutations xs = concat $ getZipList $ 
--                   (ZipList $ map (\x -> map (x:)) xs) <*> (ZipList $ map permutations $ rm xs) 

somewhere :: a -> [a] -> [[a]]
somewhere x []     = [[x]]
somewhere x (y:ys) = (x:y:ys) : ((y:) <$> somewhere x ys)

permutations :: [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) = concat $ somewhere x <$> permutations xs

--                
-- The fundamental intuition here is that if you have a list [1,2,3], then the permutations
-- on that list are 
-- map (1:) (permutations [2,3]) ++ map (2:) (permutations [1,3]) ++ map (3:) (permutations [1,2])