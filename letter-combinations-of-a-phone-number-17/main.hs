import Data.Char
import qualified Data.Array as A
import Control.Arrow

combos :: [[Char]] -> [String]
combos []     = [[]]
combos (x:xs) = (:) <$> x <*> combos xs

letters :: Char -> [Char]
letters d = let index = ord d - ord '2' in
            let array = A.listArray (0,7) $ ["abc","def","ghi","jkl","mno","pqrs","tuv","wxyz"] in
                array A.! index

letterCombinations :: String -> [String]
letterCombinations = map letters >>> combos