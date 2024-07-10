import Control.Arrow
import Data.Char
import Data.List
import Data.Function

s1 = "3+2*2"
s2 = " 3/2 "
s3 = " 3+5 / 2 "

tokenize :: String -> [String]
tokenize ""     = []
tokenize (x:xs)
      | isSpace x       = tokenize xs
      | elem x "+-*/"   = [x] : tokenize xs
      | otherwise       = let (a,b) = span isDigit $ x:xs
                          in  a : tokenize b

-- calc :: [String] -> Int
calc []         = 0
calc (x:[])     =  read x :: Int
calc (x:"*":xs) = (read x :: Int) * calc xs
calc (x:"/":xs) = (read x :: Int) `quot` calc xs
calc (x:"+":xs) = (read x :: Int) + calc xs
calc (x:"-":xs) = (read x :: Int) - calc xs


merge :: (a -> a -> Bool)  -> [a] -> [[a]]
merge _ []       = []
merge f [x]      = [[x]]
merge f (x:y:zs)   
     | f x y     = applyFirst (x:) $ merge f $ y:zs
     | otherwise = [x] : (merge f $ y:zs)
          where applyFirst :: (a -> a) -> [a] -> [a]
                applyFirst _ []     = []
                applyFirst f (x:xs) = f x : xs 

calculate = tokenize >>> merge (on (||) (`elem` ["/","*"])) 
                     >>> fmap (\xs -> if   length xs >= 3 
                                      then show $ calc xs
                                      else head xs)
                     >>> calc