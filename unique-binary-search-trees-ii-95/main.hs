import Control.Applicative
import qualified Data.MemoCombinators as Memo

data Tree = Leaf | Branch Tree Tree deriving (Eq, Show)

generate :: Int -> [Tree]
generate 0 = [Leaf]
generate n = concat $ fmap 
             (\m -> liftA2 Branch (generate m) 
                                  (generate $ n - 1 - m))
             [0..n-1]

