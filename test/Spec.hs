import Lib
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "tests"
  [ testCase "dummy" $
      1 @?= 1
  , testCase "already normalized" $ do
      let x = TmTrue :: Term String
      smallStep x @?= Nothing
  , testCase "lam" $ do
      let x = app1 (lam1 "z" (TmVar "z")) TmTrue
      smallStep x @?= Just TmTrue
  ]
