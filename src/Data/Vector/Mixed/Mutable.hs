{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Vector.Mixed.Mutable
  ( MVector
  , IOVector
  , STVector

  -- * Accessors

  -- ** Length information
  , length, null

  -- ** Extracting subvectors
  , slice, init, tail, take, drop, splitAt
  , unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop

  -- ** Overlapping
  , overlaps

  -- * Construction
  , replicateM, move, unsafeMove

  -- ** Initialisation
  , new, unsafeNew, replicate, clone

  -- ** Growing
  , grow, unsafeGrow

  -- ** Restricting memory usage
  , clear

  -- * Accessing individual elements
  , read, write, swap
  , unsafeRead, unsafeWrite, unsafeSwap

  -- * Modifying vectors

  -- ** Filling and copying
  , set, copy, unsafeCopy

  ) where

import Control.Monad (liftM)
import Control.Monad.Primitive
import qualified Data.Vector.Generic.Mutable as G
import Data.Vector.Mixed.Internal
import Prelude hiding (length, null, replicate, reverse, map, read, take, drop, init, tail, splitAt)

type IOVector = MVector RealWorld

type STVector = MVector

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: G.MVector u a => u s a -> Int
length = G.length
{-# INLINE length #-}

-- | Check whether the vector is empty
null :: G.MVector u a => u s a -> Bool
null = G.null
{-# INLINE null #-}

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it.
slice :: Mixed u v a => Int -> Int -> u s a -> MVector s a
slice i j m = mmix (G.slice i j m)
{-# INLINE slice #-}

take :: Mixed u v a => Int -> u s a -> MVector s a
take i m = mmix (G.take i m)
{-# INLINE take #-}

drop :: Mixed u v a => Int -> u s a -> MVector s a
drop i m = mmix (G.drop i m)
{-# INLINE drop #-}

splitAt :: Mixed u v a => Int -> u s a -> (MVector s a, MVector s a)
splitAt i m = case G.splitAt i m of
  (l,r) -> (mmix l, mmix r)
{-# INLINE splitAt #-}

init :: Mixed u v a => u s a -> MVector s a
init m = mmix (G.init m)
{-# INLINE init #-}

tail :: Mixed u v a => u s a -> MVector s a
tail m = mmix (G.tail m)
{-# INLINE tail #-}

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: Mixed u v a => Int  -- ^ starting index
            -> Int  -- ^ length of the slice
            -> u s a
            -> MVector s a
unsafeSlice i j m  = mmix (G.unsafeSlice i j m)
{-# INLINE unsafeSlice #-}

unsafeTake :: Mixed u v a => Int -> u s a -> MVector s a
unsafeTake i m = mmix (G.unsafeTake i m)
{-# INLINE unsafeTake #-}

unsafeDrop :: Mixed u v a => Int -> u s a -> MVector s a
unsafeDrop i m = mmix (G.unsafeDrop i m)
{-# INLINE unsafeDrop #-}

unsafeInit :: Mixed u v a => u s a -> MVector s a
unsafeInit m = mmix (G.unsafeInit m)
{-# INLINE unsafeInit #-}

unsafeTail :: Mixed u v a => u s a -> MVector s a
unsafeTail m = mmix (G.unsafeTail m)
{-# INLINE unsafeTail #-}

-- Overlapping
-- -----------

-- Check whether two vectors overlap.
overlaps :: (Mixed u v a, Mixed u' v' a) => u s a -> u' s a -> Bool
overlaps m n = G.overlaps (mmix m) (mmix n)
{-# INLINE overlaps #-}

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: PrimMonad m => Int -> m (MVector (PrimState m) a)
new = G.new
{-# INLINE new #-}

-- | Create a mutable vector of the given length. The length is not checked.
unsafeNew :: PrimMonad m => Int -> m (MVector (PrimState m) a)
unsafeNew n = liftM mboxed (G.unsafeNew n)
{-# INLINE unsafeNew #-}

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: PrimMonad m => Int -> a -> m (MVector (PrimState m) a)
replicate n a = liftM mboxed (G.replicate n a)
{-# INLINE replicate #-}

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: PrimMonad m => Int -> m a -> m (MVector (PrimState m) a)
replicateM n m = liftM mboxed (G.replicateM n m)
{-# INLINE replicateM #-}

-- | Create a copy of a mutable vector.
clone :: (PrimMonad m, Mixed u v a) => u (PrimState m) a -> m (MVector (PrimState m) a)
clone m = liftM mmix (G.clone m)
{-# INLINE clone #-}

-- Growing
-- -------

-- | Grow a vector by the given number of elements. The number must be
-- positive.
grow :: (PrimMonad m, Mixed u v a) => u (PrimState m) a -> Int -> m (MVector (PrimState m) a)
grow m n = liftM mmix (G.grow m n)
{-# INLINE grow #-}

-- | Grow a vector by the given number of elements. The number must be
-- positive but this is not checked.
unsafeGrow :: (PrimMonad m, Mixed u v a) => u (PrimState m) a -> Int -> m (MVector (PrimState m) a)
unsafeGrow m n = liftM mmix (G.unsafeGrow m n)
{-# INLINE unsafeGrow #-}

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors.
clear :: (PrimMonad m, G.MVector u a) => u (PrimState m) a -> m ()
clear = G.clear
{-# INLINE clear #-}

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position.
read :: (PrimMonad m, G.MVector u a) => u (PrimState m) a -> Int -> m a
read = G.read
{-# INLINE read #-}

-- | Replace the element at the given position.
write :: (PrimMonad m, G.MVector u a) => u (PrimState m) a -> Int -> a -> m ()
write = G.write
{-# INLINE write #-}

-- | Swap the elements at the given positions.
swap :: (PrimMonad m, G.MVector u a) => u (PrimState m) a -> Int -> Int -> m ()
swap = G.swap
{-# INLINE swap #-}


-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, G.MVector u a) => u (PrimState m) a -> Int -> m a
unsafeRead = G.unsafeRead
{-# INLINE unsafeRead #-}

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: (PrimMonad m, G.MVector u a) => u (PrimState m) a -> Int -> a -> m ()
unsafeWrite = G.unsafeWrite
{-# INLINE unsafeWrite #-}

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap :: (PrimMonad m, G.MVector u a) => u (PrimState m) a -> Int -> Int -> m ()
unsafeSwap = G.unsafeSwap
{-# INLINE unsafeSwap #-}

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, G.MVector u a) => u (PrimState m) a -> a -> m ()
set = G.set
{-# INLINE set #-}

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, Mixed u v a, Mixed u' v' a) => u (PrimState m) a -> u' (PrimState m) a -> m ()
copy dst src = G.copy (mmix dst) (mmix src)
{-# INLINE copy #-}

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy
  :: (PrimMonad m, Mixed u v a, Mixed u' v' a)
  => u (PrimState m) a   -- ^ target
  -> u' (PrimState m) a   -- ^ source
  -> m ()
unsafeCopy dst src = G.unsafeCopy (mmix dst) (mmix src)
{-# INLINE unsafeCopy #-}

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
move :: (PrimMonad m, Mixed u v a, Mixed u' v' a) => u (PrimState m) a -> u' (PrimState m) a -> m ()
move dst src = G.move (mmix dst) (mmix src)
{-# INLINE move #-}

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
unsafeMove :: (PrimMonad m, Mixed u v a, Mixed u' v' a)
  => u (PrimState m) a   -- ^ target
  -> u' (PrimState m) a   -- ^ source
  -> m ()
unsafeMove dst src = G.unsafeMove (mmix dst) (mmix src)
{-# INLINE unsafeMove #-}
