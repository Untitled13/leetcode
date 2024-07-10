import qualified Data.HashSet as Set
-- import qualified Data.Map as Map
import qualified Data.HashMap.Strict as Map
import Data.Function
import Control.Applicative
import Data.List

import Data.List.Split
-- remove1 :: String -> [String]
-- remove1 []     = []
-- remove1 (x:xs) = xs : (map (x:) $ remove1 xs)

-- insert1 :: String -> [String]
-- insert1 []     = map (\x -> [x]) ['a'..'z']
-- insert1 (x:xs) = liftA2 (:) ['a'..'z'] [x:xs] ++ map (x:) (insert1 xs)

-- replace1 :: String -> [String]
-- replace1 []     = []
-- replace1 (x:xs) = liftA2 (:) ['a'..'z'] [xs] ++ map (x:) (replace1 xs)

-- edit :: String -> [String]
-- edit xs = concat $ [remove1, insert1, replace1] <*> [xs]

-- dist :: String -> String -> Int
-- dist xs ys = nWhile (not . elem ys) $ iterate (>>= edit) [xs]

-- nWhile :: (a -> Bool) -> [a] -> Int
-- nWhile _ []      = 0
-- nWhile f (x:xs)
--      | f x       = 1 + nWhile f xs
--      | otherwise = 0




-- remove1 :: String -> Set.HashSet String
-- remove1 []     = Set.empty
-- remove1 (x:xs) = Set.insert xs $ Set.map (x:) $ remove1 xs

-- insert1 :: [Char] -> String -> Set.HashSet String 
-- insert1 _  []     = Set.fromList $ fmap (\x -> [x]) $ ['a'..'z']
-- insert1 cs (x:xs) = Set.fromList (liftA2 (:) cs [x:xs]) `Set.union` (Set.map (x:) (insert1 cs xs))

-- replace1 :: [Char] -> String -> Set.HashSet String
-- replace1 _  []     = Set.empty
-- replace1 cs (x:xs) = Set.fromList (liftA2 (:) cs [xs]) `Set.union` Set.map (x:) (replace1 cs xs)

-- edit :: [Char] -> String -> Set.HashSet String
-- edit ys xs = Set.unions $ [remove1, insert1 ys, replace1 ys] <*> [xs]

-- -- edits ::  Set.H String -> Set.HashSet String 
-- -- edits = foldl Set.union Set.empty . Set.map edit

-- edits :: String -> Map.HashMap String Int -> Map.HashMap String Int
-- edits target = foldl Map.union Map.empty . Map.mapWithKey (\s i -> Map.map (const (succ i)) $ Set.toMap $ edit target s)

-- update :: String -> Map.HashMap String Int -> Map.HashMap String Int
-- update target m = Map.unionWith min m (edits target m) 

-- dist :: String -> String -> Int
-- dist xs ys
--      | on (>=) (length . nub) xs ys = (Map.!ys) $ applyUntil (Map.member ys) (update (nub ys)) $ Map.singleton xs 0
--      | otherwise                    = dist ys xs
     

-- applyUntil :: (a -> Bool) -> (a -> a) -> a -> a
-- applyUntil p f x
--      | p x       = x
--      | otherwise = applyUntil p f (f x)

dist :: String -> String -> Int
dist []     ys     = length ys
dist xs     []     = length xs
dist (x:xs) (y:ys)
     | x == y      = dist xs ys
     | otherwise   = 1 + minimum [dist (x:xs) ys, dist xs (y:ys), dist xs ys]

-- dist :: Int -> Int -> Int
-- dist i==xs.len j   = ys.len() - j

(...) = (.) . (.)

k = 15

toInts :: String -> [Int]
toInts = reverse . map (\x -> read x :: Int) . chunksOf k

carryA :: Int -> [Int] -> [Int]
-- carryA 0 xs     = xs
carryA c []     = [c]
carryA c (x:xs) = ((c + x) `mod` (10^k)) : carryA ((c + x) `div` (10^k)) xs

add :: [Int] -> [Int] -> [Int]
add [] ys         = ys
add xs []         = xs
add (x:xs) (y:ys) = (:) ((x + y) `mod` (10^k)) $ carryA ((x + y) `div` (10^k)) $ add xs ys

carryM :: Int -> [Int] -> [Int]
carryM c []     = []
carryM c (x:xs) = let res = c * x
                      dig = res `mod` (10^k)
                      car = res `div` (10^k)
                   in (dig:) $ carryA car $ carryM c xs

mult :: [Int] -> [Int] -> [Int]
mult ys     [] = []
mult ys (x:xs) =  add (carryM x ys) $ (0 :) $ mult xs ys

-- --    2
-- -- * 19
-- --  8 : carryA 1 $ mult 1 2
--     

-- 12
-- 34


-- (10*x1 + x0) (10* y1  + y0) = 100x1y1 + 10 x1 y0 + 10 y1 x0 + x0 y0
   
-- mult [] ys = ys
-- mult xs [] = xs
-- mult (x:xs) (y:ys) 
-- -- 4,2,0,1
-- 2

x = 5279034857249308572390857
y = 5347829347589234758937598

-- test :: Integer -> Integer -> Bool
-- test x y = (==x*y) $ foldl (\acc x -> acc * 10^k + x) 0 $ on mult (toInts . show) x y



