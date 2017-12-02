module A_Basics where

-- =====================================
--           FUNCTIONS
-- =====================================

-- basic function
add :: Integer -> Integer -> Integer
add a b = a + b

-- look ma, no types
add1 a b = a + b

-- no need for args either
add2 = (+)

-- lambda
mul = \a b -> a * b

add12 = let a = 1
            b = 2
        in a + b

add34 =  a + b where
          a = 3
          b = 4

gt2 n = if n > 2 then True else False

-- partial application
addThree :: Integer -> Integer
addThree = add 3

addThree2 = (3 +)

-- function composition
mulTwo = (2 *)


mulTwoThenAddThree :: Integer -> Integer
mulTwoThenAddThree = addThree . mulTwo

-- =====================================
--           TUPLES
-- =====================================

t1 = (1, 2)
t2 = (1, 'a')

-- pattern matching
tfst (x, _) = x
tsnd (_, y) = y

tfst2 tpl = case tpl of
    (x, _) -> x

-- =====================================
--           LISTS
-- =====================================

l1 :: [Integer]
l1 = [1, 2, 3]

-- what it actually means
l2 = 1 : 2 : 3 : []

-- can only contain the same type
-- le = ['c',  "this doesnt work"]

lx = head l1
lxs = tail l1
lidx = l1 !! 2

-- =====================================
--           MAYBE
-- =====================================
msv :: Maybe Int
msv = Just 3

mn :: Maybe Int
mn = Nothing

value mb = case mb of
  Just v  -> v
  Nothing -> 0

-- =====================================
--           DATA
-- =====================================

-- type alias
type Name = String
type Age = Integer

-- basic data types
data Person = Person Name Age

-- name (Person n _) = n
-- age (Person _ a) = a

data Person2 = Person2 { name :: Name, age :: Age }

p1 :: Person2
p1 = Person2 "Joe" 42

increaseAge p@(Person2 n a) = p { age = a + 1 }


data Color = Red | Green | Blue

data BTree a = Leaf
             | Node (BTree a) a  (BTree a)

toColorString :: Color -> String
toColorString Red   = "red"
toColorString Green = "green"
toColorString Blue  = "blue"

data Option a = None
              | Some a



-- cannot reassign
-- no global state
-- cannot mutate value
