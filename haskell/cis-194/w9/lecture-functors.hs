class MyFunctor f where
  fmap :: (a -> b) -> f a -> f b

instance MyFunctor Maybe where
  fmap _ Nothing  = Nothing
  fmap h (Just a) = Just (h a)

instance MyFunctor [] where
  fmap _ [] = []
  fmap f (x:xs) = f x : fmap f xs

instance MyFunctor IO where
  fmap f ioa = ioa >>= (\a -> return (f a))
  -- alternatively
  -- fmap f ioa = ioa >>= (return . f)


