import Data.Function
import qualified Data.List as List
import Data.Matrix
import Control.Arrow
import Control.Applicative
import Data.Semigroup

-- (.:) f g   = \x y   -> f (g x y)      -- = (.) . (.)
-- (..:) f g  = \x y z -> f (g x y z)    -- = (f .) .: g = ((.) . (.) . (.))
-- upon  f g h = \x y   -> f (g x) (h y)  -- = 
-- split f g h = \x     -> f (g x) (h x)  -- = uncurry f . g &&& h = (uncurry f .) .. (&&&)

-- newtype Lines = Lines [String]

-- instance Show Lines where
--      show (Lines xs) = intercalate "\n" xs




-- n_queens :: Int -> [Matrix Boolean]
-- n_queens = map ( ... (,)) $ flip n_queens' $ matrix n n (const Nothing)

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = [(x:), id] <*> subsets xs 

-- possibilities :: Int -> Matrix (Maybe Boolean) -> [Matrix (Maybe Boolean)]
-- possibilities 0 = id
-- possibilities n m = m 

rm :: [Int] -> [[Int]]
rm [] = [[]]
rm (x:xs) =  xs : (map (x:) $ rm xs)

p :: [Int] -> [[Int]]
p [] = []
p [x] = [[x]]
p xs = concat $ getZipList $ (ZipList $ map (\x -> map (x:)) xs) <*> (ZipList $ map p $ rm xs)

