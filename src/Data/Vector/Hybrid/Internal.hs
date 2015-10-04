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

module Data.Vector.Hybrid.Internal
  ( MVector(..)
  , Vector(..)
  ) where

import Control.Monad
import Data.Monoid
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Generic as G
#if MIN_VERSION_vector(0,11,0)
import Data.Vector.Fusion.Bundle as Stream
#else
import Data.Vector.Fusion.Stream as Stream
#endif
import Data.Data
import Prelude hiding ( length, null, replicate, reverse, map, read, take, drop, init, tail )
import Text.Read

data MVector :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> * where
  MV :: !(u s a) -> !(v s b) -> MVector u v s (a, b)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
 deriving Typeable

#define Typeable1 Typeable

#else

-- custom Typeable
instance (Typeable2 u, Typeable2 v) => Typeable2 (MVector u v) where
  typeOf2 (_ :: MVector u v s ab) = mkTyConApp mvectorTyCon [typeOf2 (undefined :: u s a), typeOf2 (undefined :: v s b)]

mvectorTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
mvectorTyCon = mkTyCon3 "hybrid-vectors" "Data.Vector.Hybrid.Internal" "MVector"
#else
mvectorTyCon = mkTyCon "Data.Vector.Hybrid.Internal.MVector"
#endif

#endif

instance (GM.MVector u a, GM.MVector v b) => GM.MVector (MVector u v) (a, b) where
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV ks vs)= GM.basicInitialize ks >> GM.basicInitialize vs
  {-# INLINE basicInitialize #-}
#endif
  basicLength (MV ks _) = GM.basicLength ks
  {-# INLINE basicLength #-}
  basicUnsafeSlice s e (MV ks vs) = MV (GM.basicUnsafeSlice s e ks) (GM.basicUnsafeSlice s e vs)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV ks vs) (MV ks' vs') = GM.basicOverlaps ks ks' || GM.basicOverlaps vs vs'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = liftM2 MV (GM.basicUnsafeNew n) (GM.basicUnsafeNew n)
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate n (k,v) = liftM2 MV (GM.basicUnsafeReplicate n k) (GM.basicUnsafeReplicate n v)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV ks vs) n = liftM2 (,) (GM.basicUnsafeRead ks n) (GM.basicUnsafeRead vs n)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV ks vs) n (k,v) = do
    GM.basicUnsafeWrite ks n k
    GM.basicUnsafeWrite vs n v
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV ks vs) = do
    GM.basicClear ks
    GM.basicClear vs
  {-# INLINE basicClear #-}
  basicSet (MV ks vs) (k,v) = do
    GM.basicSet ks k
    GM.basicSet vs v
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV ks vs) (MV ks' vs') = do
    GM.basicUnsafeCopy ks ks'
    GM.basicUnsafeCopy vs vs'
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV ks vs) (MV ks' vs') = do
    GM.basicUnsafeMove ks ks'
    GM.basicUnsafeMove vs vs'
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV ks vs) n = liftM2 MV (GM.basicUnsafeGrow ks n) (GM.basicUnsafeGrow vs n)
  {-# INLINE basicUnsafeGrow #-}

-- hybrid vectors
data Vector :: (* -> *) -> (* -> *) -> * -> * where
  V :: !(u a) -> !(v b) -> Vector u v (a, b)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
 deriving Typeable
#else

-- custom Typeable
instance (Typeable1 u, Typeable1 v) => Typeable1 (Vector u v) where
  typeOf1 (_ :: Vector u v ab) = mkTyConApp vectorTyCon [typeOf1 (undefined :: u a), typeOf1 (undefined :: v b)]

vectorTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
vectorTyCon = mkTyCon3 "hybrid-vectors" "Data.Vector.Hybrid.Internal" "Vector"
#else
vectorTyCon = mkTyCon "Data.Vector.Hybrid.Internal.Vector"
#endif

#endif

type instance G.Mutable (Vector u v) = MVector (G.Mutable u) (G.Mutable v)

instance (G.Vector u a, G.Vector v b) => G.Vector (Vector u v) (a, b) where
  basicUnsafeFreeze (MV ks vs) = liftM2 V (G.basicUnsafeFreeze ks) (G.basicUnsafeFreeze vs)
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V ks vs) = liftM2 MV (G.basicUnsafeThaw ks) (G.basicUnsafeThaw vs)
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V ks _) = G.basicLength ks
  {-# INLINE basicLength #-}
  basicUnsafeSlice i j (V ks vs) = V (G.basicUnsafeSlice i j ks) (G.basicUnsafeSlice i j vs)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V ks vs) n = liftM2 (,) (G.basicUnsafeIndexM ks n) (G.basicUnsafeIndexM vs n)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV ks vs) (V ks' vs') = do
    G.basicUnsafeCopy ks ks'
    G.basicUnsafeCopy vs vs'
  {-# INLINE basicUnsafeCopy #-}
  elemseq (V ks vs) (k,v) b = G.elemseq ks k (G.elemseq vs v b)
  {-# INLINE elemseq #-}

instance (G.Vector u a, G.Vector v b, c ~ (a, b)) => Monoid (Vector u v c) where
  mappend = (G.++)
  {-# INLINE mappend #-}
  mempty = G.empty
  {-# INLINE mempty #-}
  mconcat = G.concat
  {-# INLINE mconcat #-}

instance (G.Vector u a, G.Vector v b, Show a, Show b, c ~ (a, b)) => Show (Vector u v c) where
  showsPrec = G.showsPrec

instance (G.Vector u a, G.Vector v b, Read a, Read b, c ~ (a, b)) => Read (Vector u v c) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

instance (Data a, Data b, Typeable1 u, Typeable1 v, G.Vector u a, G.Vector v b, c ~ (a, b)) => Data (Vector u v c) where
  gfoldl       = G.gfoldl
  toConstr _   = error "toConstr" -- TODO: virtual constructor
  gunfold _ _  = error "gunfold"  -- TODO: virtual constructor
  dataTypeOf _ = G.mkType "Data.Vector.Hybrid.Vector"
  dataCast1    = G.dataCast


instance (G.Vector u a, G.Vector v b, Eq a, Eq b, c ~ (a, b)) => Eq (Vector u v c) where
  xs == ys = Stream.eq (G.stream xs) (G.stream ys)
  {-# INLINE (==) #-}

  xs /= ys = not (Stream.eq (G.stream xs) (G.stream ys))
  {-# INLINE (/=) #-}


-- See http://trac.haskell.org/vector/ticket/12
instance (G.Vector u a, G.Vector v b, Ord a, Ord b, c ~ (a, b)) => Ord (Vector u v c) where
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

