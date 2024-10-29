{-# LANGUAGE CPP, FlexibleInstances, GADTs        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures               #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE TypeOperators                                            #-}
{-# LANGUAGE UndecidableInstances                                     #-}

module Data.Vector.Hybrid.Internal
  ( MVector(..)
  , Vector(..)
  ) where

import           Control.Monad
import qualified Data.Foldable               as F
import           Data.Monoid
import           Data.Semigroup
import Data.Vector.Fusion.Bundle as Stream
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM

import Data.Data
import Prelude   hiding (drop, init, length, map, null, read, replicate,
                  reverse, tail, take)
import Text.Read

data MVector :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> * where
  MV :: !(u s a) -> !(v s b) -> MVector u v s (a, b)

instance (GM.MVector u a, GM.MVector v b) => GM.MVector (MVector u v) (a, b) where
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
  basicInitialize (MV ks vs) = GM.basicInitialize ks >> GM.basicInitialize vs
  {-# INLINE basicInitialize #-}

-- hybrid vectors
data Vector :: (* -> *) -> (* -> *) -> * -> * where
  V :: !(u a) -> !(v b) -> Vector u v (a, b)

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

instance (G.Vector u a, G.Vector v b, c ~ (a, b)) => Semigroup (Vector u v c) where
  (<>) = (G.++)
  sconcat = mconcat . F.toList

instance (G.Vector u a, G.Vector v b, c ~ (a, b)) => Monoid (Vector u v c) where
#if !(MIN_VERSION_base(4,11,0))
  mappend = (G.++)
  {-# INLINE mappend #-}
#endif
  mempty = G.empty
  {-# INLINE mempty #-}
  mconcat = G.concat
  {-# INLINE mconcat #-}

instance (G.Vector u a, G.Vector v b, Show a, Show b, c ~ (a, b)) => Show (Vector u v c) where
  showsPrec = G.showsPrec

instance (G.Vector u a, G.Vector v b, Read a, Read b, c ~ (a, b)) => Read (Vector u v c) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

instance (Data a, Data b, Typeable u, Typeable v, G.Vector u a, G.Vector v b, c ~ (a, b)) => Data (Vector u v c) where
  gfoldl       = G.gfoldl
  toConstr _   = error "toConstr" -- TODO: virtual constructor
  gunfold _ _  = error "gunfold"  -- TODO: virtual constructor
  dataTypeOf _ = mkNoRepType "Data.Vector.Hybrid.Vector"
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

