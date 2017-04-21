module OwenT
  where
import           Numeric.Integration.TanhSinh
import           Data.Number.Erf  (normcdf)

integrandT :: Double -> Double -> Double
integrandT h x = exp (-(h*h)*(1+x*x)/2) /(1+x*x)

integralT :: Double -> Double -> [Result]
integralT h = trap (integrandT h) 0

isInfinite :: Double -> Bool
isInfinite x = abs x == 1/0

owenT :: Double -> Double -> Double
owenT h a | a < 0 = - (owenT h (-a))
          | a == 1/0 = (1 - normcdf (abs(h)))/2
          | a <= 1 = result (last $ integralT h a) /2/pi
          | otherwise = (normcdf h + normcdf (a*h))/2 -
                          normcdf h * normcdf (a*h) -
                            owenT (a*h) (1/a)
