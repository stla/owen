module OwenT2
  where
import           Math.Gamma                   (lnGamma)
import           Data.Number.Erf  (normcdf)


crossprod :: Int -> Double -> Double -> (Double, Double)
crossprod i h a
  | i==0 = (1, (1-k)*a)
  | otherwise = (x+u, y+v)
  where k = exp(-h*h/2)
        (x, y) = crossprod (i-1) h a
        u = exp (2*j*log h - j*log 2 - lnGamma (j+1))
        v = (1 - k*(x+u)) / jk * a^(2*i+1)
        jk = fromIntegral $ if odd i then -(2*i+1) else 2*i+1
        j = fromIntegral i

owenT01 :: Double -> Double -> Double
owenT01 h a =
  if h>8
    then atan a * exp (-0.5 * h*h*a / atan a) *
      (1 + 0.00868 * (h*a)^4) / twopi;
    else
      (atan a - b)/twopi
    where b = snd $ crossprod 50 h a
          twopi = 2*3.14159265358979323846;

owenT2 :: Double -> Double -> Double
owenT2 h a | a < 0 = - (owenT2 h (-a))
           | a == 1/0 = (1 - normcdf (abs h))/2
           | a <= 1 = owenT01 h a
           | otherwise = (normcdf h + normcdf (a*h))/2 -
                          normcdf h * normcdf (a*h) - owenT01 (a*h) (1/a)
