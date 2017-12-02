module B_Typeclasses where

-- adds type: Num a => a -> a -> a

class StringConvertable a where
  toString :: a -> String

instance StringConvertable Integer where
  toString i = show i

instance StringConvertable Char where
  toString i = show i

-- constraints
class StringConvertable a => Jsonifyable a where
  toJson :: a -> String

greet :: StringConvertable a => a -> String
greet x = "Hello " ++ toString x
