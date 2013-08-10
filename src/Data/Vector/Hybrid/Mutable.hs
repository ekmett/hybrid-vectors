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

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

module Data.Vector.Hybrid.Mutable
  ( MVector
  , IOVector
  , STVector

  -- * Accessors

  -- ** Length information
  , length, null

  -- ** Extracting subvectors
  , slice, init, tail, take, drop
  , unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop

  -- ** Overlapping
  , overlaps

  -- * Construction

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

  -- * Unsafe Construction and deconstruction
  , unsafeFromPair, unsafeFst, unsafeSnd

  -- * Deprecated operations
  , newWith, unsafeNewWith
  ) where

import Control.Monad

import qualified Data.Vector.Generic.Mutable as G
import Control.Monad.Primitive

import Prelude hiding ( length, null, replicate, reverse, map, read, take, drop, init, tail )

import Data.Typeable

data MVector :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> * where
  MV :: !(u s a) -> !(v s b) -> MVector u v s (a, b)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
 deriving Typeable
#else

-- custom Typeable
instance (Typeable2 u, Typeable2 v) => Typeable2 (MVector u v) where
  typeOf2 (_ :: MVector u v s ab) = mkTyConApp mvectorTyCon [typeOf2 (undefined :: u s a), typeOf2 (undefined :: v s b)]

mvectorTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
mvectorTyCon = mkTyCon3 "hybrid-vectors" "Data.Vector.Hybrid.Mutable" "MVector"
#else
mvectorTyCon = mkTyCon "Data.Vector.Hybrid.Mutable.MVector"
#endif

#endif

type IOVector u v = MVector u v RealWorld
type STVector = MVector

instance (G.MVector u a, G.MVector v b) => G.MVector (MVector u v) (a, b) where
  basicLength (MV ks _) = G.basicLength ks
  {-# INLINE basicLength #-}
  basicUnsafeSlice s e (MV ks vs) = MV (G.basicUnsafeSlice s e ks) (G.basicUnsafeSlice s e vs)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV ks vs) (MV ks' vs') = G.basicOverlaps ks ks' || G.basicOverlaps vs vs'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = liftM2 MV (G.basicUnsafeNew n) (G.basicUnsafeNew n)
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate n (k,v) = liftM2 MV (G.basicUnsafeReplicate n k) (G.basicUnsafeReplicate n v)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV ks vs) n = liftM2 (,) (G.basicUnsafeRead ks n) (G.basicUnsafeRead vs n)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV ks vs) n (k,v) = do
    G.basicUnsafeWrite ks n k
    G.basicUnsafeWrite vs n v
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV ks vs) = do
    G.basicClear ks
    G.basicClear vs
  {-# INLINE basicClear #-}
  basicSet (MV ks vs) (k,v) = do
    G.basicSet ks k
    G.basicSet vs v
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV ks vs) (MV ks' vs') = do
    G.basicUnsafeCopy ks ks'
    G.basicUnsafeCopy vs vs'
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV ks vs) (MV ks' vs') = do
    G.basicUnsafeMove ks ks'
    G.basicUnsafeMove vs vs'
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV ks vs) n = liftM2 MV (G.basicUnsafeGrow ks n) (G.basicUnsafeGrow vs n)
  {-# INLINE basicUnsafeGrow #-}


-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: G.MVector u a => MVector u v s (a, b) -> Int
length (MV ks _) = G.length ks
{-# INLINE length #-}

-- | Check whether the vector is empty
null :: G.MVector u a => MVector u v s (a, b) -> Bool
null (MV ks _) = G.null ks
{-# INLINE null #-}

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it.
slice :: (G.MVector u a, G.MVector v b) => Int -> Int -> MVector u v s (a, b) -> MVector u v s (a, b)
slice = G.slice
{-# INLINE slice #-}

take :: (G.MVector u a, G.MVector v b) => Int -> MVector u v s (a, b) -> MVector u v s (a, b)
take = G.take
{-# INLINE take #-}

drop :: (G.MVector u a, G.MVector v b) => Int -> MVector u v s (a, b) -> MVector u v s (a, b)
drop = G.drop
{-# INLINE drop #-}

init :: (G.MVector u a, G.MVector v b) => MVector u v s (a, b) -> MVector u v s (a, b)
init = G.init
{-# INLINE init #-}

tail :: (G.MVector u a, G.MVector v b) => MVector u v s (a, b) -> MVector u v s (a, b)
tail = G.tail
{-# INLINE tail #-}

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: (G.MVector u a, G.MVector v b)
            => Int  -- ^ starting index
            -> Int  -- ^ length of the slice
            -> MVector u v s (a, b)
            -> MVector u v s (a, b)
unsafeSlice = G.unsafeSlice
{-# INLINE unsafeSlice #-}

unsafeTake :: (G.MVector u a, G.MVector v b) => Int -> MVector u v s (a, b) -> MVector u v s (a, b)
unsafeTake = G.unsafeTake
{-# INLINE unsafeTake #-}

unsafeDrop :: (G.MVector u a, G.MVector v b) => Int -> MVector u v s (a, b) -> MVector u v s (a, b)
unsafeDrop = G.unsafeDrop
{-# INLINE unsafeDrop #-}

unsafeInit :: (G.MVector u a, G.MVector v b) => MVector u v s (a, b) -> MVector u v s (a, b)
unsafeInit = G.unsafeInit
{-# INLINE unsafeInit #-}

unsafeTail :: (G.MVector u a, G.MVector v b) => MVector u v s (a, b) -> MVector u v s (a, b)
unsafeTail = G.unsafeTail
{-# INLINE unsafeTail #-}

-- Overlapping
-- -----------

-- Check whether two vectors overlap.
overlaps :: (G.MVector u a, G.MVector v b) => MVector u v s (a, b) -> MVector u v s (a, b) -> Bool
overlaps = G.overlaps
{-# INLINE overlaps #-}

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: (PrimMonad m, G.MVector u a, G.MVector v b) => Int -> m (MVector u v (PrimState m) (a, b))
new = G.new
{-# INLINE new #-}

-- | Create a mutable vector of the given length. The length is not checked.
unsafeNew :: (PrimMonad m, G.MVector u a, G.MVector v b) => Int -> m (MVector u v (PrimState m) (a, b))
unsafeNew = G.unsafeNew
{-# INLINE unsafeNew #-}

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: (PrimMonad m, G.MVector u a, G.MVector v b) => Int -> (a, b) -> m (MVector u v (PrimState m) (a, b))
replicate = G.replicate
{-# INLINE replicate #-}

-- | Create a copy of a mutable vector.
clone :: (PrimMonad m, G.MVector u a, G.MVector v b)
      => MVector u v (PrimState m) (a, b) -> m (MVector u v (PrimState m) (a, b))
clone = G.clone
{-# INLINE clone #-}

-- Growing
-- -------

-- | Grow a vector by the given number of elements. The number must be
-- positive.
grow :: (PrimMonad m, G.MVector u a, G.MVector v b)
     => MVector u v (PrimState m) (a, b) -> Int -> m (MVector u v (PrimState m) (a, b))
grow = G.grow
{-# INLINE grow #-}

-- | Grow a vector by the given number of elements. The number must be
-- positive but this is not checked.
unsafeGrow :: (PrimMonad m, G.MVector u a, G.MVector v b)
               => MVector u v (PrimState m) (a, b) -> Int -> m (MVector u v (PrimState m) (a, b))
unsafeGrow = G.unsafeGrow
{-# INLINE unsafeGrow #-}

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors. 
clear :: (PrimMonad m, G.MVector u a, G.MVector v b) => MVector u v (PrimState m) (a, b) -> m ()
clear = G.clear
{-# INLINE clear #-}

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position.
read :: (PrimMonad m, G.MVector u a, G.MVector v b)
     => MVector u v (PrimState m) (a, b) -> Int -> m (a, b)
read = G.read
{-# INLINE read #-}

-- | Replace the element at the given position.
write :: (PrimMonad m, G.MVector u a, G.MVector v b)
      => MVector u v (PrimState m) (a, b) -> Int -> (a, b) -> m ()
write = G.write
{-# INLINE write #-}

-- | Swap the elements at the given positions.
swap :: (PrimMonad m, G.MVector u a, G.MVector v b)
     => MVector u v (PrimState m) (a, b) -> Int -> Int -> m ()
swap = G.swap
{-# INLINE swap #-}


-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, G.MVector u a, G.MVector v b)
           => MVector u v (PrimState m) (a, b) -> Int -> m (a, b)
unsafeRead = G.unsafeRead
{-# INLINE unsafeRead #-}

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: (PrimMonad m, G.MVector u a, G.MVector v b)
            =>  MVector u v (PrimState m) (a, b) -> Int -> (a, b) -> m ()
unsafeWrite = G.unsafeWrite
{-# INLINE unsafeWrite #-}

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap
    :: (PrimMonad m, G.MVector u a, G.MVector v b)
    => MVector u v (PrimState m) (a, b) -> Int -> Int -> m ()
unsafeSwap = G.unsafeSwap
{-# INLINE unsafeSwap #-}

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, G.MVector u a, G.MVector v b)
    => MVector u v (PrimState m) (a, b) -> (a, b) -> m ()
set = G.set
{-# INLINE set #-}

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, G.MVector u a, G.MVector v b)
     => MVector u v (PrimState m) (a, b) -> MVector u v (PrimState m) (a, b) -> m ()
copy = G.copy
{-# INLINE copy #-}

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: (PrimMonad m, G.MVector u a, G.MVector v b)
           => MVector u v (PrimState m) (a, b)   -- ^ target
           -> MVector u v (PrimState m) (a, b)   -- ^ source
           -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- Unsafe composition and decomposition
-- ------------------------------------

unsafeFromPair :: u s a -> v s b -> MVector u v s (a, b)
unsafeFromPair = MV
{-# INLINE unsafeFromPair #-}

unsafeFst :: MVector u v s (a, b) -> u s a
unsafeFst (MV ks _) = ks
{-# INLINE unsafeFst #-}

unsafeSnd :: MVector u v s (a, b) -> v s b
unsafeSnd (MV _ vs) = vs
{-# INLINE unsafeSnd #-}

-- Deprecated functions
-- --------------------

-- | /DEPRECATED/ Use 'replicate' instead
newWith :: (PrimMonad m, G.MVector u a, G.MVector v b) => Int -> (a, b) -> m (MVector u v (PrimState m) (a, b))
newWith = G.replicate
{-# INLINE newWith #-}

-- | /DEPRECATED/ Use 'replicate' instead
unsafeNewWith :: (PrimMonad m, G.MVector u a, G.MVector v b) => Int -> (a, b) -> m (MVector u v (PrimState m) (a, b))
unsafeNewWith = G.replicate
{-# INLINE unsafeNewWith #-}

{-# DEPRECATED newWith, unsafeNewWith "Use replicate instead" #-}
