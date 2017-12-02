module G_BetterErrorHandling where

import           Control.Lens
import           Data.Char
import           Data.Validation

type Password = String

data PasswordError = TooShort
                   | NoNumeric
                   | NoUppercase
                   deriving (Show)

validate :: Bool -> a -> e -> AccValidation [e] a
validate validationResult succRes err = if validationResult
  then _Success # succRes
  else _Failure # [err]

validateLength :: Password -> AccValidation [PasswordError] Password
validateLength pass = validate (length pass > 8) pass TooShort

validateNumeric :: Password -> AccValidation [PasswordError] Password
validateNumeric pass = validate (any isNumber pass) pass NoNumeric

validateUppercase :: Password -> AccValidation [PasswordError] Password
validateUppercase pass = validate (any isUpper pass) pass TooShort

validatePassword :: Password -> AccValidation [PasswordError] Password
validatePassword pass = pass
                     <$ validateLength pass
                     <* validateNumeric pass
                     <* validateUppercase pass
