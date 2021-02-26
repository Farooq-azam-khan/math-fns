module SimpsonsRule where 
import Debug.Trace 
type LowerBound = Float
type UpperBound = Float 

type Segments = Int 

simpson_one_third :: (Float -> Float) -> LowerBound -> UpperBound -> Float
simpson_one_third f a b = 
  let 
    x1 = (b-a)/(fromIntegral 2)
  in 
    ((b-a)/6) * ((f a) + 4 * f(x1) + f(b) )

simpson_one_third_comp :: (Float -> Float) -> LowerBound -> UpperBound -> Segments -> Float 
simpson_one_third_comp f a b n = 
  let 
    h= (b-a) / (fromIntegral n)
    part1 = (b-a) 
    odd_vals = filter odd [1..(n-1)]
    the_mapped_odd_vals =map (\i -> f $ (fromIntegral i) * h + a) odd_vals 
    odd_sum =sum the_mapped_odd_vals
    even_sum = sum $ map (\i -> f $ (fromIntegral i) * h + a) $ filter even [2..(n-2)] 
    part2 = (f a) + (f b) + 4 * odd_sum + 2 * even_sum 
    pt2 = (part2 / (3 * ( fromIntegral n)))
  in 
    part1 * pt2

