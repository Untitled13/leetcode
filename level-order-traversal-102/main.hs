
import Data.List

data Tree a = Branch a (Tree a) (Tree a) | Leaf

lot :: Tree a -> [[a]]
lot Leaf           = []
lot (Branch a x y) = [a] : zip' (lot x) (lot y)

zip' :: (Monoid a) => [a] -> [a] -> [a]
zip' [] ys         = ys
zip' xs []         = xs
zip' (x:xs) (y:ys) = (x <> y) : zip' xs ys