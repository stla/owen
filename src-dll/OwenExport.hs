{-# LANGUAGE ForeignFunctionInterface #-}
module OwenExport
  where
import           Foreign
import           Foreign.C
import           OwenQ     (owenQ, owenQ1, owenQ2)
import           OwenT     (owenT)
import OwenTboost (owenTboost)
import           Student   (pStudent)

foreign export ccall owenTexport :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
owenTexport :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
owenTexport h a result = do
  h <- peek h
  a <- peek a
  let out = owenT (realToFrac h) (realToFrac a)
  poke result (realToFrac out)

foreign export ccall owenTboostR :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
owenTboostR :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
owenTboostR h a result = do
  h <- peek h
  a <- peek a
  let out = owenTboost (realToFrac h) (realToFrac a)
  poke result (realToFrac out)

foreign export ccall owenQexport :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
  Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
owenQexport :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
  Ptr CDouble -> Ptr CDouble -> IO ()
owenQexport nu t delta a b result = do
  nu <- peek nu
  delta <- peek delta
  t <- peek t
  a <- peek a
  b <- peek b
  let out = owenQ (realToFrac nu) (realToFrac t) (realToFrac delta)
              (realToFrac a) (realToFrac b)
  poke result (realToFrac out)

foreign export ccall owenQ1export :: Ptr CInt -> Ptr CDouble -> Ptr CDouble ->
  Ptr CDouble -> Ptr CDouble -> IO ()
owenQ1export :: Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
  Ptr CDouble -> IO ()
owenQ1export nu t delta r result = do
  nu <- peek nu
  let df = (fromIntegral nu) :: Int
  delta <- peek delta
  t <- peek t
  r <- peek r
  let out = owenQ1 df (realToFrac t) (realToFrac delta) (realToFrac r)
  poke result (realToFrac out)

foreign export ccall owenQ2export :: Ptr CInt -> Ptr CDouble -> Ptr CDouble ->
  Ptr CDouble -> Ptr CDouble -> IO ()
owenQ2export :: Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
  Ptr CDouble -> IO ()
owenQ2export nu t delta r result = do
  nu <- peek nu
  let df = (fromIntegral nu) :: Int
  delta <- peek delta
  t <- peek t
  r <- peek r
  let out = owenQ2 df (realToFrac t) (realToFrac delta) (realToFrac r)
  poke result (realToFrac out)

foreign export ccall pStudentExport :: Ptr CDouble -> Ptr CInt -> Ptr CDouble ->
  Ptr CDouble -> IO ()
pStudentExport :: Ptr CDouble -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO ()
pStudentExport q nu delta result = do
  nu <- peek nu
  let df = (fromIntegral nu) :: Int
  delta <- peek delta
  q <- peek q
  let out = pStudent (realToFrac q) df (realToFrac delta)
  poke result (realToFrac out)
