
import Control.Arrow
import Data.List

-- substrings :: [a] -> [[a]]
-- substrings []  = []
-- substrings (x:xs) = initials (x:xs) ++ substrings xs

-- initials :: [a] -> [[a]]
-- initials []     = [[]]
-- initials (x:xs) = ([]:) $ (x:) <$> initials xs

-- substrings "abcde" = 'a': <$>

palindrome :: Eq a => [a] -> Bool
palindrome = uncurry (==) . (&&&) reverse id

partitions :: [a] -> [[[a]]]
partitions []     = []
partitions [x]    = [[[x]]]
partitions (x:xs) = (([x]:) <$> partitions xs) ++ ((\(y:ys) -> (x:y):ys) <$> partitions xs)

partition :: (Eq a) => [a] -> [[[a]]]
partition = filter (all palindrome) . partitions

-- partitions "abc" = [["a","b","c"],
--                     ["a","bc"],
--                     ["ab","c"],
--                     ["abc"]]

-- partitions "bc"  = [["b","c"],
--                     ["bc"]]

-- compress :: String -> String 
