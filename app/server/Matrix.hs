{-# LANGUAGE FlexibleContexts #-}

module Matrix where

import Data.Array.IO (readArray, writeArray, getElems, MArray(newArray), IOUArray)
import Control.Monad (forM)

data Matrix a = Matrix {
  size :: (Int, Int),
  content :: IOUArray Int a
}

newMatrix :: MArray IOUArray a IO => Int -> Int -> a -> IO (Matrix a)
newMatrix n m e = Matrix (n, m) <$> newArray (1, n * m) e

readMatrix :: MArray IOUArray a IO => Matrix a -> Int -> Int -> IO a
readMatrix (Matrix (_, m) arr) x y = readArray arr ((x - 1) * m + y)

readMatrixRow :: MArray IOUArray a IO => Matrix a -> Int -> IO [a]
readMatrixRow mat row = forM [1..m] $ readMatrix mat row
  where m = snd $ size mat

elemsMatrix :: MArray IOUArray a IO => Matrix a -> IO [[a]]
elemsMatrix (Matrix (_, m) arr) = makeGroupsOf m <$> getElems arr
  where
    makeGroupsOf m [] = []
    makeGroupsOf m l = let (mElems, l') = splitAt m l in mElems : makeGroupsOf m l'

writeMatrix :: MArray IOUArray a IO => Matrix a -> Int -> Int -> a -> IO ()
writeMatrix (Matrix (_, m) arr) x y = writeArray arr ((x - 1) * m + y)

updateMatrix :: MArray IOUArray a IO => Matrix a -> Int -> Int -> (a -> a) -> IO ()
updateMatrix (Matrix (_, m) arr) x y f = writeArray arr i . f =<< readArray arr i
  where i = (x - 1) * m + y 
