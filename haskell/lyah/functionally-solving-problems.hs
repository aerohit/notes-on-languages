import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunc [] . words
    where foldingFunc (x:y:ys) "*" = (x * y) : ys
          foldingFunc (x:y:ys) "+" = (x + y) : ys
          foldingFunc (x:y:ys) "-" = (y - x) : ys
          foldingFunc (x:y:ys) "/" = (y / x) : ys
          foldingFunc (x:y:ys) "^" = (y ** x) : ys
          foldingFunc (x:xs) "ln"  = (ln x) : ys
          foldingFunc xs "sum"     = [sum xs]
          foldingFunc xs numberString    = read numberString : xs
