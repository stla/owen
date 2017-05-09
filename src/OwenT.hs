module OwenT
  where
import           Data.Number.Erf (normcdf)
import           Math.Gamma      (lnFactorial)

owenSeries :: Int -> Double -> Double -> (Double, Double)
owenSeries i h a
  | i==0 = (1, (1-k)*a)
  | otherwise = (z, y+v)
  where k = exp(-h*h/2)
        (x, y) = owenSeries (i-1) h a
        u = exp (2*j*log h - j*log 2 - lnFactorial i)
        z = x+u
        v = (1 - k*z) / jk * a^(2*i+1)
        jk = fromIntegral $ if odd i then -(2*i+1) else 2*i+1
        j = fromIntegral i

owenT01 :: Double -> Double -> Double
owenT01 h a =
  if h>8
    then atan a * exp (-0.5 * h*h*a / atan a) *
      (1 + 0.00868 * (h*a)^4) / twopi;
    else
      (atan a - b)/twopi
    where b = snd $ owenSeries 50 h a
          twopi = 2*3.14159265358979323846;

owenT :: Double -> Double -> Double
owenT h a | isInfinite (abs h) = 0
          | a < 0 = - (owenT h (-a))
          | a == 1/0 = (1 - normcdf (abs h))/2
          | a <= 1 = owenT01 h a
          | otherwise = (normcdf h + normcdf (a*h))/2 -
                          normcdf h * normcdf (a*h) - owenT01 (a*h) (1/a)

-- test
-- owenT 0.1 0.1
-- Wolfram: 0.015783380517183599183765627
