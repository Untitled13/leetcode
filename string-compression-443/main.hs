import Control.Arrow
import Data.List

compress :: String -> String
compress = group >>> map ((&&&) head length)
                 >>> (>>= \(c,l) -> (c, l `mod` 9) : replicate (l `div` 9) (c,9))
                 >>> map (second show)
                 >>> map (uncurry (:))
                 >>> concat