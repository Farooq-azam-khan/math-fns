module LinearRegression where

get_coeff :: [(Float, Float)] -> (Float, Float)
get_coeff values = (get_a1 values, get_a0 values)  

get_a1 :: [(Float, Float)] -> Float 
get_a1 values = (n * xy_sum - x_sum * y_sum) / (n * xsq_sum - x_sum^2)
              where 
                n = (fromIntegral (length values) :: Float)
                xy_sum = sum $ map (\(x,y) -> x*y) values
                x_sum = sum $ map (\(x,y) -> x) values 
                y_sum = sum $ map (\(x,y) -> y) values 
                xsq_sum = sum $ map (\(x, y) ->  x^2) values 

get_a0 :: [(Float, Float)] -> Float
get_a0 values = y_mean - (get_a1 values) * x_mean
              where 
                y_mean = (sum $ map (\(x,y) -> y) values) / (fromIntegral $ length values) :: Float
                x_mean = (sum $ map (\(x,y) -> x) values) / (fromIntegral $ length values) :: Float
