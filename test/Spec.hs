import           Test.Hspec

import           C_RecursionSpec as CRS

main :: IO ()
main = hspec $ do
  CRS.spec
