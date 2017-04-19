module Student
  (pStudent)
  where
import           Data.Number.Erf  (normcdf)
import           OwenT            (owenT)
import           Student.Internal (evenMsequence, oddMsequence)

pnorm :: Double -> Double
pnorm = normcdf

pStudent :: Double -> Int -> Double -> Double
pStudent q df delta =
  if odd df
    then
      pnorm (-delta*sB) + 2 * owenT (delta*sB) a + 2 * sum oddMs
    else
      pnorm (-delta) + sum evenMs * sqrt(2*pi)
  where nu = fromIntegral df
        a = signum q * sqrt (q*q / nu)
        b = nu / (nu + q*q)
        sB = sqrt b
        oddMs = oddMsequence df a b delta
        evenMs = evenMsequence df a b delta
