import Data.Fix
import Eval
import Lib
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tmTrue :: Term () String String
tmTrue = embed RTmTrue

tyBool :: FixType
tyBool = Fix TyBool

tests = testGroup "tests"
  [ testCase "dummy" $
      1 @?= 1
  , testCase "already normalized" $ do
      smallStep tmTrue @?= Nothing
  , testCase "lam" $ do
      let x = app1 (lam1 "z" tyBool (var "z")) tmTrue
      smallStep x @?= Just tmTrue
  ]
