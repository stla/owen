module OwenQ
    (owenQ, owenQ1, owenQ2)
  where
import           Data.Number.Erf              (normcdf)
import           Math.Gamma                   (lnGamma, p)
import           Numeric.Integration.TanhSinh
import           OwenQ.Internal               (extractHeven, extractHodd,
                                               extractMeven, extractModd, seqs)
import           OwenT                        (owenT)
import           Student                      (pStudent)

pnorm :: Double -> Double
pnorm = normcdf

integrandQ :: Double -> Double -> Double -> Double -> Double
integrandQ nu delta t x = pnorm(t*x /sqrt nu - delta) *
  exp((nu-1)*log x - x*x/2 - ((nu/2) - 1) * log 2 - lnGamma (nu/2))

integralQ :: Double -> Double -> Double -> Double -> Double -> [Result]
integralQ nu delta t = trap (integrandQ nu delta t)

owenQ :: Double -> Double -> Double -> Double -> Double -> Double
owenQ nu t delta a b = result (last $ integralQ nu delta t a b)

owenQ1 :: Int -> Double -> Double -> Double -> Double
owenQ1 nu t delta r
  | isInfinite delta =
    if delta>0
      then
        0
      else
        p (df/2) (r^2/2)
  | even nu =
    pnorm(-delta) + sqrt(2*pi)*(sum evenM + sum evenH)
  | otherwise =
          2*(sum oddM + sum oddH) + pnorm r - 2*owenT r ((a*r-delta)/r) -
            2*owenT (delta*sB) ((ab-r/delta)/b) + 2*owenT (delta*sB) a -
              if delta >=0 then 1 else 0
      where evenM     = extractMeven sequences
            evenH     = extractHeven sequences
            oddM      = extractModd sequences
            oddH      = extractHodd sequences
            sequences = seqs (nu-2) a b delta r
            a         = signum t * sqrt(t*t /  df)
            b         = df / (df+t*t)
            ab        = if isInfinite t then 0 else a*b
            sB        = sqrt b
            df        = fromIntegral nu

owenQ2 :: Int -> Double -> Double -> Double -> Double
owenQ2 nu t delta r = pStudent t nu delta - owenQ1 nu t delta r
