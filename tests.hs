import Test.Hspec
import Test.QuickCheck

import Choose
import Numerical 

main :: IO ()
main = hspec $ 
  describe "all tests" $ do 
    describe "test choose function" $ do 
    it "check oupt when k = 0" $ do 
      (Choose.choose 3 0) `shouldBe` (1 :: Int)
    describe "error tests" $ do 
     it "should return zero when true and relative are the same" $ do 
      (Numerical.true_relative_error 1 1 `shouldBe` (0 :: Float))

    it "should return zero when pres val is same as curr val" $ do 
      (approximate_error 1 1 `shouldBe` (0 :: Float))

    it "should get the right stoping criterion" $ do 
      (stopping_criterion 0) `shouldBe` (0.5 * 10*10 :: Float)

