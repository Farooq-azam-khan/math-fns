module Numerical where 

true_relative_error :: Float -> Float -> Float 
true_relative_error tv ap = (tv - ap) / tv 

approximate_error :: Float -> Float -> Float
approximate_error pres_v prev_val = (pres_v - prev_val) / pres_v 

stopping_criterion n = 0.5 * 10^(2-n)

