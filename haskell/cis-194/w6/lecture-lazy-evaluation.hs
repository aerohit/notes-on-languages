(&&!!) :: Bool -> Bool -> Bool
True &&!! x  = x
False &&!! _ = False

(&&!) :: Bool -> Bool -> Bool
True &&! True   = True
True &&! False  = False
False &&! True  = False
False &&! False = False

isFalse = False &&!!  (34^9784346 > 34987345)
isFalseButTakesLonger = False &&! (34^9784346 > 34987345)

isFalseAndWillEvaluate = False &&!!  (head [] == 'x')
willCrash = False &&! (head [] == 'x')

if' :: Bool -> a -> a -> a
if' True x _  = x
if' False _ y = y
