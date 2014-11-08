{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Mutable vectors that support automatic dynamic resizing.
--
-- These live in ST and are intended for append and update workloads.
module Data.Vector.Resizable (
  RVector,
  new,
  read,
  write,
  append,
  size,
  capacity,
  ifoldM,
  freeze
  ) where

import Prelude hiding ( read )

import Control.Monad.ST ( ST )
import Data.STRef
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Generic as V

data RVector base s a =
  RVector { rvSize :: STRef s Int
          , rvCapacity :: STRef s Int
          , rvVector :: STRef s (base a)
          }

-- | Allocate a new empty vector with the given initial capacity.
new :: (MV.MVector base a) => Int -> ST s (RVector (base s) s a)
new cap0 = do
  sref <- newSTRef 0
  cref <- newSTRef cap0
  v <- MV.new cap0
  vref <- newSTRef v
  return RVector { rvSize = sref
                 , rvCapacity = cref
                 , rvVector = vref
                 }

-- | Read from the vector (checked)
read :: (MV.MVector base a) => RVector (base s) s a -> Int -> ST s a
read rv ix = do
  v <- readSTRef (rvVector rv)
  MV.read v ix

-- | Write to the vector (checked)
write :: (MV.MVector base a) => RVector (base s) s a -> Int -> a -> ST s ()
write rv ix !val = do
  v <- readSTRef (rvVector rv)
  MV.write v ix val

-- | Append a value to the vector safely, returning the index the item
-- was inserted at.
append :: (MV.MVector base a) => RVector (base s) s a -> a -> ST s Int
append rv !val = do
  cap <- readSTRef (rvCapacity rv)
  sz <- readSTRef (rvSize rv)
  v <- readSTRef (rvVector rv)
  let ix = sz
  case cap > sz of
    True -> do
      MV.write v ix val
    False -> do
      v' <- MV.grow v cap
      MV.write v' ix val
      writeSTRef (rvVector rv) v'
      writeSTRef (rvCapacity rv) (cap * 2)
  modifySTRef' (rvSize rv) (+1)
  return ix

size :: (MV.MVector base a) => RVector (base s) s a -> ST s Int
size = readSTRef . rvSize

capacity :: (MV.MVector base a) => RVector (base s) s a -> ST s Int
capacity = readSTRef . rvCapacity

freeze :: (V.Vector base a) => RVector (V.Mutable base s) s a -> ST s (base a)
freeze rv = do
  sz <- size rv
  v <- readSTRef (rvVector rv)
  let v' = MV.take sz v
  V.freeze v'

ifoldM :: (MV.MVector base a)
       => (b -> Int -> a -> ST s b)
       -> b
       -> RVector (base s) s a
       -> ST s b
ifoldM f seed rv = do
  v <- readSTRef (rvVector rv)
  sz <- readSTRef (rvSize rv)
  go v seed 0 sz
  where
    go v !acc !n sz
      | n < sz = do
        elt <- MV.read v n
        acc' <- f acc n elt
        go v acc' (n+1) sz
      | otherwise = return acc
