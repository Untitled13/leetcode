import Control.Applicative

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = [(x:), id] <*> subsets xs 