module OwenQ
  (owenQ, owenQ1)
  where
import           Data.Number.Erf              (normcdf)
import           Math.Gamma                   (lnGamma)
import           Numeric.Integration.TanhSinh
import           OwenQ.Internal               (extractHeven, extractHodd,
                                               extractMeven, extractModd, seqs)
import           OwenT                        (owenT)

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
owenQ1 nu t delta r =
  if even nu
    then
      pnorm(-delta) + sqrt(2*pi)*(sum evenM + sum evenH)
    else
      2*(sum oddM + sum oddH) + pnorm r - 2*owenT r ((a*r-delta)/r) -
        2*owenT (delta*sB) ((delta*a*b-r)/b/delta) + 2*owenT (delta*sB) a - 1
  where evenM = extractMeven sequences
        evenH = extractHeven sequences
        oddM = extractModd sequences
        oddH = extractHodd sequences
        sequences = seqs (nu-2) a b delta r
        a = sqrt(t*t /  df)
        b = df / (df+t*t)
        sB = sqrt b
        df = fromIntegral nu
