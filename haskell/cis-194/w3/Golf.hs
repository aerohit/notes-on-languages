module Golf where

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | y > x && y > z = y:localMaxima (y:z:zs)
  | otherwise      = localMaxima (y:z:zs)
localMaxima _     = []
