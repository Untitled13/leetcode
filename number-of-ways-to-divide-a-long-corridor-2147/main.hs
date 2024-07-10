

import Data.List
import Data.Function
import Control.Arrow


divide :: String -> Int
divide ""  = 1
divide ws = let xs      = dropWhileAcc (\acc c -> case c of 
                                                       'S' -> acc + 1
                                                       _  -> acc) (<2) 0 ws
                (ys,zs) = span (=='P') xs
             in (1 + length ys) * divide zs

divide' s
     | odd   $ length $ filter (=='S') s = 0
     | (==2) $ length $ filter (=='S') s = 1
     | otherwise                         = divide s

s0 = "SSPPSPS"
s1 = "PPSPSP"

dropWhileAcc :: (a -> x -> a) -> (a -> Bool) -> a -> [x] -> [x]
dropWhileAcc _ _ _ [] = []
dropWhileAcc f g a (x:xs)
          | g $ f a x = dropWhileAcc f g (f a x) xs
          | otherwise = xs