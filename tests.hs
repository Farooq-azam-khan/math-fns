import Test.Hspec
import Test.QuickCheck

import Choose
import Numerical 
import LinearRegression

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
    describe "Linear regression" $ do 
      it "calculates the right a1 value" $ do 
         (abs (lin_reg_tst - 19.470) < 1e-3) `shouldBe` True 
      it "calculates a1" $ do 
        ((abs (test_a0 - actual_a0)) < 1e-3) `shouldBe` True
      
      it "test the tuple for a1, a0" $ do 
        (test_tuple `shouldBe` (True, True))

test_tuple = let 
             (a1, a0) = get_coeff [(10, 25), (20, 70), (30, 380), (40, 550), (50, 610), (60, 1220), (70, 830), (80, 1450)]
             in 
                (abs (a1 - 19.470) < 1e-3, abs (a0 - actual_a0) < 1e-3)

lin_reg_tst =  get_a1 [(10, 25), (20, 70), (30, 380), (40, 550), (50, 610), (60, 1220), (70, 830), (80, 1450)]

actual_a0 = (-234.2857)
test_a0 =  get_a0 [(10, 25), (20, 70), (30, 380), (40, 550), (50, 610), (60, 1220), (70, 830), (80, 1450)]
