module OwenQ.Internal
  where
import           Data.Number.Erf              (normcdf)
import           Data.Ratio                   ((%))

dnorm :: Double -> Double
dnorm x = exp(-x*x/2)/sqrt(2*pi)

pnorm :: Double -> Double
pnorm = normcdf

seqs :: Int -> Double -> Double -> Double -> Double -> [(Rational, Double, Double, Double)]
seqs (-2) a b d r = [(1, 0, 0, 0)]
seqs (-1) a b d r =
  seqs (-2) a b d r ++
                    [
                      (1, 0, 0, 0)
                    ]
seqs 0 a b d r =
  seqs (-1) a b d r ++
                 [
                   (1 % 2
                  , 0
                  , -dnorm r * pnorm (a*r-d)
                  , asB*dnorm(d*sB)*(pnorm(d*asB)-pnorm((d*ab-r)/sB)))
                 ]
               where sB  = sqrt b
                     asB = if isInfinite a then signum a else a*sB
                     ab  = if isInfinite a then 0 else a*b
seqs 1 a b d r =
  previous ++
   [(
     2 % 3
   , ab * r * dnorm r * dnorm (a*r-d) / 2
   , r * h0
   , ab * (d*m0 + dnorm(d*sB)*(dnorm(d*asB)-dnorm((d*ab-r)/sB)))
   )]
  where previous       = seqs 0 a b d r
        (_, _, h0, m0) = last previous
        sB             = sqrt b
        asB            = if isInfinite a then signum a else a*sB
        ab             = if isInfinite a then 0 else a*b
seqs k a b d r = previous ++
  [(
     1 / (toRational k + 2) / am1
   , fromRational am1 * r * lm1
   , fromRational am2 * r * hm1
   , (1 - 1 / fromIntegral k) * b * (fromRational am4 * d * a * mm1 + mm2) - lm1
  )]
                    where previous             = seqs (k-1) a b d r
                          (am1, lm1, hm1, mm1) = last previous
                          (am2, _, _, mm2)     = previous !! k
                          (am4, _, _, _) = previous !! (k-2)

extractMeven :: [(Rational, Double, Double, Double)] -> [Double]
extractMeven sequences =
  map (sequenceM !!) indices
    where indices   = [i | i <- [0..l], even i]
          sequenceM = map (\(_,_,_,x) -> x) (drop 2 sequences)
          l         = length sequenceM - 1

extractHeven :: [(Rational, Double, Double, Double)] -> [Double]
extractHeven sequences =
  map (sequenceH !!) indices
    where indices   = [i | i <- [0..l], even i]
          sequenceH = map (\(_,_,x,_) -> x) (drop 2 sequences)
          l         = length sequenceH - 1

extractModd :: [(Rational, Double, Double, Double)] -> [Double]
extractModd sequences =
  map (sequenceM !!) indices
    where indices   = [i | i <- [1..l], odd i]
          sequenceM = map (\(_,_,_,x) -> x) (drop 2 sequences)
          l         = length sequenceM - 1

extractHodd :: [(Rational, Double, Double, Double)] -> [Double]
extractHodd sequences =
  map (sequenceH !!) indices
    where indices   = [i | i <- [1..l], odd i]
          sequenceH = map (\(_,_,x,_) -> x) (drop 2 sequences)
          l         = length sequenceH - 1
