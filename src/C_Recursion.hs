module C_Recursion where

fact 0 = 1
fact n = n * fact n - 1

--  convert array to string
stringify :: Show a => [a] -> [String]
stringify []       = []
stringify (x : xs) = show x : stringify xs

stringify2 :: Show a => [a] -> [String]
stringify2 = map show

sumList :: [Integer] -> Integer
sumList []       = 0
sumList (x : xs) = x + sumList xs

-- generic sum

class Reducible r where -- Foldable
  reduce :: (a -> b -> b) -> b -> r a -> b

instance Reducible [] where
  reduce _ seed []        =  seed
  reduce fn seed (x : xs) = fn x (reduce fn seed xs)

class Addable a where -- Monoid
  madd :: a -> a -> a
  mzero :: a

instance Addable Integer where
  madd = (+)
  mzero = 0

instance Addable [a] where
  madd = (++)
  mzero = []

instance Addable a => Addable (Maybe a) where
  madd (Just x) (Just y) = Just $ madd x y -- equivalent to Just (madd x y)
  madd _ _               = Nothing

  mzero = Just mzero

sumAll :: (Reducible r, Addable a) => r a -> a
sumAll xs = reduce madd mzero xs
