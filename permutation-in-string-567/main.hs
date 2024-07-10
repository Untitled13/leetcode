import qualified Data.Map as M
import Control.Arrow
import Data.Function
import Data.List
import Data.List.Split
import Debug.Trace

freq :: Ord a => [a] -> M.Map a Int
freq = fmap (flip (,) 1) >>> M.fromListWith (+)

-- inclusion_ :: M.Map Char Int -> String -> Bool
-- inclusion_ m [] = null m
-- inclusion_ m (s:ss)
--      | null m         = False
--      | s `M.member` m =  inclusion_ (M.update 
--                                       (\n -> if   n - 1 > 0
--                                              then Just $ n - 1
--                                              else Nothing) s m) ss
--                       || inclusion_ m ss
--      | otherwise    = inclusion_ m ss 

-- inclusion :: String -> String -> Bool
-- inclusion s =  
pass :: Ord a => [a] -> [a] -> [[a]]
pass s = let m = freq s
         in  splitWhen (not . (`M.member` m))
             >>> filter (on (<=) length s)

-- inclusion :: Ord a => [a] -> [a] -> Bool
-- inclusion :: String -> String -> Bool
inclusion s r = let -- m :: M.Map a Int
                    m            = freq s
                    n :: Int
                    n            = length s
                    -- processed :: [String]
                    processed    =  splitWhen (not . (`M.member` m)) >>> filter (\xs -> length xs >= n) $ r
                    -- f :: [a] -> Bool
                    f t = let (begin, end) = splitAt n t
                              -- m' :: M.Map a Int
                              m'           =  M.unionWith (+) (freq begin) (fmap negate m)
                              -- f' :: (M.Map a Int, [a], Bool)
                              f'           = foldl (\(m,y:ys,p ) x -> 
                                                      if   p || all (==0) m
                                                      then (m,y:ys,True)
                                                      else (traceShowId $ M.adjust (flip (-) 1) y $ M.insertWith (+) x 1 $ m,ys, False)
                                                 ) (m',t,all (==0) m') end
                         in (\(m,_,p) ->  p || all (==0) m) $ traceShowId f'
                in any f processed