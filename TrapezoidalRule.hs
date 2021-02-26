module TrapezoidalRule where 

type LowerBound = Float
type UpperBound = Float 
trap_rule :: (Float -> Float) -> LowerBound -> UpperBound -> Float
trap_rule f a b  = (b - a) *( ((f a) + (f b)) / (2 :: Float))

type Segments = Int 
composite_trap :: (Float -> Float) -> LowerBound -> UpperBound -> Segments -> Float
composite_trap f a b n = 
        let
          h = (b-a)/(fromIntegral n)
          middle_part = sum $ map (\i -> f ((fromIntegral i)*h+a) ) [1..(n-1)] 
        in 
            (b-a) * (( (f a) + 2 * middle_part + (f b)  ) / ((fromIntegral n) * 2))

