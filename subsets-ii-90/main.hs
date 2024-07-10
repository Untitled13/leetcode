import qualified Data.Map as M
import Data.Char
import Data.List

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = M.toList (M.fromListWith (+) [(x,1) | x <- xs])

subsets' :: (Ord a) => [(a, Int)] -> [[a]]
subsets' []         = [[]]
subsets' ((x,0):xs) = subsets' xs
subsets' ((x,n):xs) = (take (succ n) $ iterate ((x:) .) id) <*> subsets' xs

subsets :: (Ord a) => [a] -> [[a]]
subsets = subsets' . frequency

decodings :: String -> Int
decodings = length . filter id . map (all match) . groups
     where match :: String -> Bool
           match (c:[])     = c /= '0'
           match ('0':xs)   = False
           match ('1':xs)   = True
           match ('2':y:[]) = y `elem` "0123456"
           match _          = False

groups :: String -> [[String]]
groups []     = [[]]
groups [x]    = [[[x]]]
groups (x:y:xs) = fmap ([x]:) (groups (y:xs)) ++ fmap ([x,y]:) (groups xs)

-- generate :: Int -> [String]
-- generate 0 = [""]
-- -- generate 1 = ["()"]
-- generate n = nub $ [\s -> "(" ++ s ++ ")", ("()"++), (++"()")] <*> generate (pred n)


