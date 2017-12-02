module F_Monad where

data Box a = Empty
           | Full a

class MMonad f where
  flatMap :: f a -> (a -> f b) -> f b
  ret :: a -> f a

instance MMonad Box where
  flatMap Empty _         = Empty
  flatMap (Full value) fn = fn value

  ret v = Full v

div3 _ 0       = Empty
div3 num denom = Full $ num `div` denom

listM = do
  a <- [1, 2, 3]
  return $ a * 2

maybeM1 = do
  x <- Just 1
  y <- Just 2
  return $ x + y

maybeM2 = do
  x <- Just 1
  y <- Nothing
  return $ x + y
