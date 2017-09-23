import Criterion.Main
import Criterion.Types
import OwenT
import OwenTintegration
import OwenTboost

o1 :: (Double, Double) -> Double
o1 (h,a) = owenT h a

o2 :: (Double, Double) -> Double
o2 (h,a) = owenTintegration h a

-- main = defaultMain [
--   bgroup "owent" [ bench "series"  $ nf (owenT 0.5) 0.5
--                , bench "integration"  $ nf (owenTintegration 0.5) 0.5
-- --               , bench "boost"  $ whnf (owenTboost 0.5) 0.5
--                ]
--   ]

--
main :: IO ()
main = defaultMainWith
  defaultConfig { resamples = 10 }
  [ bench "series"  $ nf o1 (0.5, 0.5)
  , bench "integration"  $ nf o2 (0.5, 0.5)
  ]
-- stack exec -- ghc -O --make src/Benchmark.hs
-- ./Benchmark --output benchmark.html
