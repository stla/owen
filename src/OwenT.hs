module OwenT
  where
import           Numeric.Integration.TanhSinh

integrandT :: Double -> Double -> Double
integrandT h x = exp (-h*h*(1+x*x)/2) /(1+x*x)

integralT :: Double -> Double -> [Result]
integralT h = trap (integrandT h) 0

owenT :: Double -> Double -> Double
owenT h a = result (last $ integralT h a) /2/pi
