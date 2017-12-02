module E_ErrorHandling where

import           Control.Monad
import           Data.Char

div1 _ 0       = Nothing
div1 num denom = Just $ num `div` denom

div2 _ 0       = Left "Division by zero!"
div2 num denom = Right $ num `div` denom

divBy2 num = map (num `div2`)

r0 = divBy2 50 [1, 2, 3, 4, 5, 6, 7]
r1 = divBy2 50 [1, 2, 3, 4, 5, 6, 0, 7]
r2 = divBy2 50 [1, 2, 3, 4, 5, 6, 0, 7, 0]


-- password validation

type Password = String

data PasswordError = TooShort
                   | NoNumeric
                   | NoUppercase
                   deriving (Show)

validateLengthE :: Password -> Either PasswordError Password
validateLengthE pass = if length pass > 8
  then Right pass
  else Left TooShort

validateNumericE :: Password -> Either PasswordError Password
validateNumericE pass = if any isNumber pass
  then Right pass
  else Left TooShort

validateUppercaseE :: Password -> Either PasswordError Password
validateUppercaseE pass = if any isUpper pass
  then Right pass
  else Left TooShort

validatePasswordThePainfulWay pass = case validateLengthE pass of
  Left e -> Left e
  Right vp0 -> case validateNumericE vp0 of
    Left e -> Left e
    Right vp1 -> case validateUppercaseE vp1 of
      Left e    -> Left e
      Right vp1 -> Right vp1

-- Monad !!!
validatePasswordTheLessPainfulWay pass = validateLengthE pass
                                     >>= \vp0 -> validateNumericE vp0
                                     >>= \vp1 -> validateUppercaseE vp1

validatePasswordTheRightWay pass = do
  vp0 <- validateLengthE pass
  vp1 <- validateNumericE vp0
  vp2 <- validateUppercaseE vp1
  return vp2

-- Kleisli Arrow (composition for monads)
validatePasswordTheAscendedWay = validateLengthE
                             >=> validateNumericE
                             >=> validateUppercaseE
