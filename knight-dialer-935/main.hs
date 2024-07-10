
import Control.Applicative
import Control.Arrow
import Data.List
import Data.Function
import qualified Data.HashSet as Set
import qualified Data.Map as Map

indices  :: [(Int,Int)]
indices  = (1,-1) : liftA2 (,) [0..2] [0..2]

counts :: Map.Map (Int,Int) Int
counts = Map.fromList $ map (flip (,) 1) indices

pm = (<*>) [(+), flip (-)]

moves :: (Int,Int) -> [(Int,Int)]
moves = (<*>) (on (liftA2 (***)) pm [2] [1]
            ++ on (liftA2 (***)) pm [1] [2]) . (\x -> [x])

step :: Map.Map (Int,Int) Int -> Map.Map (Int,Int) Int
step counts = Map.mapWithKey (\p n -> (`mod` overflow) $ sum $ 
              map (\p -> Map.findWithDefault 0 p counts) $ moves p) counts

dialer :: Int -> Int
dialer n = (`mod` overflow) $ sum $ foldr (.) id (replicate (n-1) step) counts

overflow = 10^9 + 7

main :: IO ()
main = getLine >>= print . dialer . read