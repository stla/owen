{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module OwenTboost
  where
import qualified Language.C.Inline.Cpp as C
import System.IO.Unsafe (unsafePerformIO)

C.context C.cppCtx

C.include "<iostream>"
C.include "<boost/math/special_functions/owens_t.hpp>"

ttest :: Double
ttest = realToFrac $ unsafePerformIO $ [C.exp| double { boost::math::owens_t(1.1, 1.1) } |]

owenTboost :: Double -> Double -> Double
owenTboost _h _a = realToFrac $ unsafePerformIO $ [C.exp| double { boost::math::owens_t($(double h), $(double a)) } |]
                   where h = realToFrac _h :: C.CDouble
                         a = realToFrac _a :: C.CDouble
