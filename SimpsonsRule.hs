module SimpsonsRule where 
type LowerBound = Float
type UpperBound = Float 

simpson_one_third :: (Float -> Float) -> LowerBound -> UpperBound -> Float
simpson_one_third f a b = 
  let 
    x1 = (b-a)/(fromIntegral 2)
  in 
    ((b-a)/6) * ((f a) + 4 * f(x1) + f(b) )
