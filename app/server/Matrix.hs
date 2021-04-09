{-# LANGUAGE FlexibleContexts #-}

module Matrix where

import Data.Array.IO (readArray, writeArray, MArray(newArray), IOUArray)

data Matrix a = Matrix {
  size :: (Int, Int),
  content :: IOUArray Int a
}

newMatrix :: MArray IOUArray a IO => Int -> Int -> a -> IO (Matrix a)
newMatrix n m e = Matrix (n, m) <$> newArray (1, n * m) e

readMatrix :: MArray IOUArray a IO => Matrix a -> Int -> Int -> IO a
readMatrix (Matrix (n, _) arr) x y = readArray arr (x * n + y)

writeMatrix :: MArray IOUArray a IO => Matrix a -> Int -> Int -> a -> IO ()
writeMatrix (Matrix (n, _) arr) x y = writeArray arr (x * n + y)

updateMatrix :: MArray IOUArray a IO => Matrix a -> Int -> Int -> (a -> a) -> IO ()
updateMatrix (Matrix (n, _) arr) x y f = writeArray arr i . f =<< readArray arr i
  where i = x * n + y 