rpn :: [String] -> Int
rpn = flip rpn' []

rpn' :: [String] -> [Int] -> Int
rpn' []           (x:xs)   = x
rpn' ("+":inputs) (x:y:zs) = rpn' inputs (x + y : zs)
rpn' ("*":inputs) (x:y:zs) = rpn' inputs (x * y : zs)
rpn' ("-":inputs) (x:y:zs) = rpn' inputs (y - x : zs)
rpn' ("/":inputs) (x:y:zs) = rpn' inputs (quot y x : zs)
rpn' (int:inputs) zs       = rpn' inputs (read int : zs)

--this could be rewritten with a fold, but I think that code is rather
--unreadable, without benefiting much otherwise.