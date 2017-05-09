module Student.Internal
  where
import           Data.Number.Erf (normcdf)

dnorm :: Double -> Double
dnorm x = exp(-x*x/2) / sqrt (2*pi)

pnorm :: Double -> Double
pnorm = normcdf

sequences :: Int -> Double -> Double -> Double -> [(Rational, Double)]
sequences 0 a b delta = [(
                          0
                        , asb * dnorm (delta*sb) * pnorm (delta*asb)
                        )]
                        where sb  = sqrt b
                              asb = if isInfinite a then signum a else a*sb
sequences 1 a b delta =
  (0, m0) : [(
              1
            , ab * (delta * m0 + dnorm delta / sqrt(2*pi) )
            )]
    where (_, m0) = head (sequences 0 a b delta)
          ab      = if isInfinite a then 0 else a*b
sequences k a b delta =
  previous ++ [(
                1 / (toRational k - 1) / am1
              , (1 - 1 / fromIntegral k) * (fromRational am1 * delta * ab * mm1 + b*mm2)
              )]
    where previous   = sequences (k-1) a b delta
          (am1, mm1) = last previous
          (_, mm2)   = previous !! (k-2)
          ab         = if isInfinite a then 0 else a*b

oddMsequence :: Int -> Double -> Double -> Double -> [Double]
oddMsequence nu a b delta = map (mSequence !!) oddIntegers
  where mSequence = map snd $ sequences (nu-2) a b delta
        oddIntegers = [i | i <- [1..(nu-2)], odd i]

evenMsequence :: Int -> Double -> Double -> Double -> [Double]
evenMsequence nu a b delta = map (mSequence !!) evenIntegers
  where mSequence = map snd $ sequences (nu-2) a b delta
        evenIntegers = [i | i <- [0..(nu-2)], even i]
