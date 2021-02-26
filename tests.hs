import Test.Hspec
import Test.QuickCheck

import Choose
import Numerical 
import LinearRegression
import TrapezoidalRule
--newtype Tolerance = Tolerance Float 
type Tolerance = Float 

low_tolerance = 1e-3 
medium_tolerance = 1e-6
high_tolerance = 1e-9
extreme_tolerance = 1e-12

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
    describe "should test the trap rule" $ do 
      it "checks if trap rule works" $ do 
        (abs ((trap_rule a_func 0 0.8) - 0.1728) < low_tolerance) `shouldBe` True 
      it "tests the composite rule" $ do 
        (abs ((composite_trap a_func 0 0.8 2)) - 1.0688 < medium_tolerance) `shouldBe` True


test_tuple = let 
             (a1, a0) = get_coeff [(10, 25), (20, 70), (30, 380), (40, 550), (50, 610), (60, 1220), (70, 830), (80, 1450)]
             in 
                (abs (a1 - 19.470) < low_tolerance, abs (a0 - actual_a0) < low_tolerance)
a_func x = 0.2 + 25 * x - 200* x^2+ 675 * x^3 - 900 * x^4 + 400 * x^5 
lin_reg_tst =  get_a1 [(10, 25), (20, 70), (30, 380), (40, 550), (50, 610), (60, 1220), (70, 830), (80, 1450)]

actual_a0 = (-234.2857)
test_a0 =  get_a0 [(10, 25), (20, 70), (30, 380), (40, 550), (50, 610), (60, 1220), (70, 830), (80, 1450)]
