
import Data.List
import Data.Function
increasing :: [Int] -> [Int]
increasing [] = []
increasing xs = let n         = length xs `quot` 2
                    (ys,z:zs) = splitAt n xs
                 in (increasing $ filter (<z) ys)
                    ++ [z] ++ 
                    (increasing $ filter (z<) zs)

-- increasing :: [Int] -> Int
-- increasing [] = 0
-- increasing xs = let n         = length xs
--                     (ys,z:zs) = splitAt (n `quot` 2) xs
--                  in 1 + on (+) increasing (filter (<z) ys) (filter (z<) zs)