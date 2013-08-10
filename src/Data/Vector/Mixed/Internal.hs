{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

-- {-# OPTIONS_GHC -fno-method-sharing #-} -- See: http://trac.haskell.org/vector/ticket/12

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

module Data.Vector.Mixed.Internal
  ( MVector(..), mmix, mbox
  , Vector(..), mix, box
  , Mixed
  ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Generic as G
import qualified Data.Vector as B
import qualified Data.Vector.Mutable as BM
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import Data.Vector.Fusion.Stream as Stream
import Data.Data
import Prelude hiding ( length, null, replicate, reverse, map, read, take, drop, init, tail )
import Text.Read

-- | Vector doesn't provide a way to recover the type of the immutable vector from the mutable vector type
--
-- This would otherwise prevent us from finishing the implementation of 'basicUnsafeFreeze' in 'Vector'
--
-- This class captures the invariants necessary to 'hide' the choice of vector type from the user in such
-- a way that we can go from mutable vector to immutabl vector and back again.
class
  ( Typeable2 mv
  , Typeable1 v
  , mv ~ G.Mutable v
  , GM.MVector mv a
  , G.Vector v a
  ) => Mixed mv v a | mv -> v, v -> mv

instance                 Mixed B.MVector B.Vector a
instance S.Storable a => Mixed S.MVector S.Vector a
instance P.Prim a     => Mixed P.MVector P.Vector a
instance U.Unbox a    => Mixed U.MVector U.Vector a

-- | A @MVector s a@ is mutable vector that could have any vector type underneath
data MVector :: * -> * -> * where
  MV :: Mixed mv v a => !(mv s a) -> MVector s a
 deriving Typeable

mmix :: Mixed mv v a => mv s a -> MVector s a
mmix = MV

mix :: Mixed mv v a => v a -> Vector a
mix = V

mbox :: BM.MVector s a -> MVector s a
mbox = MV


box :: B.Vector a -> Vector a
box = V

newtype Id a = Id { runId :: a }

cast2 :: (Typeable2 p, Typeable2 q) => p a b -> Maybe (q a b)
cast2 x = runId <$> gcast2 (Id x)
{-# INLINE cast2 #-}

instance GM.MVector MVector a where
  basicLength (MV ks) = GM.basicLength ks
  {-# INLINE basicLength #-}
  basicUnsafeSlice s e (MV ks) = MV (GM.basicUnsafeSlice s e ks)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV as) (MV bs) = case cast2 as of
    Nothing -> True -- False could allow a composite vector that _does_ overlap internally to slip through!
    Just cs -> GM.basicOverlaps cs bs
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = liftM mbox (GM.basicUnsafeNew n)
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate n k = liftM mbox (GM.basicUnsafeReplicate n k)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV ks) n = GM.basicUnsafeRead ks n
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV ks) n k = GM.basicUnsafeWrite ks n k
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV ks) = GM.basicClear ks
  {-# INLINE basicClear #-}
  basicSet (MV ks) k = GM.basicSet ks k
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV dst) (MV src) = case cast2 dst of
      Nothing   -> go 0
      Just dst' -> GM.basicUnsafeCopy dst' src -- the types match, allow fast copy
    where
      n = GM.basicLength src
      go i
        | i < n = do
          x <- GM.basicUnsafeRead src i
          GM.basicUnsafeWrite dst i x
          go (i+1)
        | otherwise = return ()
  {-# INLINE basicUnsafeCopy #-}

  basicUnsafeMove (MV dst) (MV src) = case cast2 dst of
    Just dst' -> GM.basicUnsafeMove dst' src
    Nothing   -> do
      srcCopy <- GM.munstream (GM.mstream src)
      GM.basicUnsafeCopy dst srcCopy
  {-# INLINE basicUnsafeMove #-}

  basicUnsafeGrow (MV ks) n = liftM MV (GM.basicUnsafeGrow ks n)
  {-# INLINE basicUnsafeGrow #-}

-- mixed vectors
data Vector :: * -> * where
  V :: Mixed mv v a => !(v a) -> Vector a
 deriving Typeable

type instance G.Mutable Vector = MVector

instance G.Vector Vector a where
  basicUnsafeFreeze (MV ks) = liftM V (G.basicUnsafeFreeze ks)
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V ks) = liftM MV (G.basicUnsafeThaw ks)
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V ks) = G.basicLength ks
  {-# INLINE basicLength #-}
  basicUnsafeSlice i j (V ks) = V (G.basicUnsafeSlice i j ks)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V ks) n = G.basicUnsafeIndexM ks n
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV dst) (V src) = case cast2 dst of
      Just dst' -> G.basicUnsafeCopy dst' src
      Nothing -> go 0
    where
      !n = G.basicLength src
      go i
        | i < n = do
          x <- G.basicUnsafeIndexM src i
          GM.basicUnsafeWrite dst i x
          go (i+1)
        | otherwise = return ()
  {-# INLINE basicUnsafeCopy #-}
  elemseq (V ks) k b = G.elemseq ks k b
  {-# INLINE elemseq #-}

instance Monoid (Vector a) where
  mappend = (G.++)
  {-# INLINE mappend #-}
  mempty = G.empty
  {-# INLINE mempty #-}
  mconcat = G.concat
  {-# INLINE mconcat #-}

instance Show a => Show (Vector a) where
  showsPrec = G.showsPrec

instance Read a => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

instance Data a => Data (Vector a) where
  gfoldl       = G.gfoldl
  toConstr _   = error "toConstr" -- TODO: virtual constructor
  gunfold _ _  = error "gunfold"  -- TODO: virtual constructor
  dataTypeOf _ = G.mkType "Data.Vector.Mixed.Vector"
  dataCast1    = G.dataCast

instance Eq a => Eq (Vector a) where
  xs == ys = Stream.eq (G.stream xs) (G.stream ys)
  {-# INLINE (==) #-}

  xs /= ys = not (Stream.eq (G.stream xs) (G.stream ys))
  {-# INLINE (/=) #-}


-- See http://trac.haskell.org/vector/ticket/12
instance Ord a => Ord (Vector a) where
  {-# INLINE compare #-}
  compare xs ys = Stream.cmp (G.stream xs) (G.stream ys)

  {-# INLINE (<) #-}
  xs < ys = Stream.cmp (G.stream xs) (G.stream ys) == LT

  {-# INLINE (<=) #-}
  xs <= ys = Stream.cmp (G.stream xs) (G.stream ys) /= GT

  {-# INLINE (>) #-}
  xs > ys = Stream.cmp (G.stream xs) (G.stream ys) == GT

  {-# INLINE (>=) #-}
  xs >= ys = Stream.cmp (G.stream xs) (G.stream ys) /= LT
