{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A mixed 'Vector' lets you make a 'Vector' out of any other vector type
-- you have lying around, and all of the combinators are defined to allow
-- you to freely mix input vector type wherever possible.
--
-- This enables you to work with a mixture of boxed and unboxed data.
-----------------------------------------------------------------------------
module Data.Vector.Mixed
  (
  -- * Mixed vectors
    Vector, MVector, Mixed(..)

  -- * Accessors

  -- ** Length information
  , length
  , null

  -- ** Indexing
  , (!), (!?), head, last
  , unsafeIndex, unsafeHead, unsafeLast

  -- ** Monadic indexing
  , indexM, headM, lastM
  , unsafeIndexM, unsafeHeadM, unsafeLastM

  -- ** Extracting subvectors (slicing)
  , slice, init, tail, take, drop, splitAt
  , unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop

  -- * Construction

  -- ** Initialisation
  , empty, singleton, replicate, generate, iterateN

  -- ** Monadic initialisation
  , replicateM, generateM, create

  -- ** Unfolding
  , unfoldr, unfoldrN
  , constructN, constructrN

  -- ** Enumeration
  , enumFromN, enumFromStepN, enumFromTo, enumFromThenTo

  -- ** Concatenation
  , cons, snoc, (++), concat

  -- ** Restricting memory usage
  , force

  -- * Modifying vectors

  -- ** Bulk updates
  , (//), update, update_
  , unsafeUpd, unsafeUpdate, unsafeUpdate_

  -- ** Accumulations
  , accum, accumulate, accumulate_
  , unsafeAccum, unsafeAccumulate, unsafeAccumulate_

  -- ** Permutations
  , reverse, backpermute, unsafeBackpermute

  -- ** Safe destructive updates
  , modify

  -- * Elementwise operations

  -- ** Indexing
  , indexed

  -- ** Mapping
  , map, imap, concatMap

  -- ** Monadic mapping
  , mapM, mapM_, forM, forM_

  -- ** Zipping
  , zipWith, zipWith3, zipWith4, zipWith5, zipWith6
  , izipWith, izipWith3, izipWith4, izipWith5, izipWith6
  , zip, zip3, zip4, zip5, zip6

  -- ** Monadic zipping
  , zipWithM, zipWithM_

  -- ** Unzipping
  , unzip, unzip3, unzip4, unzip5, unzip6

  -- * Working with predicates

  -- ** Filtering
  , filter, ifilter, filterM
  , takeWhile, dropWhile

  -- ** Partitioning
  , partition, unstablePartition, span, break

  -- ** Searching
  , elem, notElem, find, findIndex, findIndices, elemIndex, elemIndices

  -- * Folding
  , foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1'
  , ifoldl, ifoldl', ifoldr, ifoldr'

  -- ** Specialised folds
  , all, any, and, or
  , sum, product
  , maximum, maximumBy, minimum, minimumBy
  , minIndex, minIndexBy, maxIndex, maxIndexBy

  -- ** Monadic folds
  , foldM, foldM', fold1M, fold1M'
  , foldM_, foldM'_, fold1M_, fold1M'_

  -- ** Monadic sequencing
  , sequence, sequence_

  -- * Prefix sums (scans)
  , prescanl, prescanl'
  , postscanl, postscanl'
  , scanl, scanl', scanl1, scanl1'
  , prescanr, prescanr'
  , postscanr, postscanr'
  , scanr, scanr', scanr1, scanr1'

  -- * Conversions

  -- ** Lists
  , toList, fromList, fromListN

  -- ** Other vector types
  , G.convert

  -- ** Mutable vectors
  , freeze, thaw, copy, unsafeFreeze, unsafeThaw, unsafeCopy
  ) where


-- import qualified Data.Vector.Hybrid.Internal as H
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Generic.New as New
import Data.Vector.Mixed.Internal
import Data.Vector.Internal.Check as Ck
import qualified Data.Vector.Fusion.Stream as Stream
import           Data.Vector.Fusion.Stream (MStream, Stream)
import qualified Data.Vector.Fusion.Stream.Monadic as MStream

-- import Control.DeepSeq ( NFData, rnf )
import Control.Monad ( liftM )
import Control.Monad.ST ( ST )
import Control.Monad.Primitive

import Prelude hiding ( length, null,
                        replicate, (++), concat,
                        head, last,
                        init, tail, take, drop, splitAt, reverse,
                        map, concatMap,
                        zipWith, zipWith3, zip, zip3, unzip, unzip3,
                        filter, takeWhile, dropWhile, span, break,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
                        all, any, and, or, sum, product, minimum, maximum,
                        scanl, scanl1, scanr, scanr1,
                        enumFromTo, enumFromThenTo,
                        mapM, mapM_, sequence, sequence_ )

#define BOUNDS_CHECK(f) (Ck.f __FILE__ __LINE__ Ck.Bounds)
#define UNSAFE_CHECK(f) (Ck.f __FILE__ __LINE__ Ck.Unsafe)

-- import Data.Typeable ( Typeable )
-- import Data.Data     ( Data(..) )
-- import Text.Read     ( Read(..), readListPrecDefault )

-- import Data.Monoid   ( Monoid(..) )
-- import qualified Control.Applicative as Applicative
-- import qualified Data.Foldable as Foldable
-- import qualified Data.Traversable as Traversable

-- Length information
-- ------------------

-- | /O(1)/ Yield the length of the vector.
length :: G.Vector v a => v a -> Int
length = G.length
{-# INLINE length #-}

-- | /O(1)/ Test whether a vector if empty
null :: G.Vector v a => v a -> Bool
null = G.null
{-# INLINE null #-}

-- Indexing
-- --------

-- | O(1) Indexing
(!) :: G.Vector v a => v a -> Int -> a
(!) = (G.!)
{-# INLINE (!) #-}

-- | O(1) Safe indexing
(!?) :: G.Vector v a => v a -> Int -> Maybe a
(!?) = (G.!?)
{-# INLINE (!?) #-}

-- | /O(1)/ First element
head :: G.Vector v a => v a -> a
head = G.head
{-# INLINE head #-}

-- | /O(1)/ Last element
last :: G.Vector v a => v a -> a
last = G.last
{-# INLINE last #-}

-- | /O(1)/ Unsafe indexing without bounds checking
unsafeIndex :: G.Vector v a => v a -> Int -> a
unsafeIndex = G.unsafeIndex
{-# INLINE unsafeIndex #-}

-- | /O(1)/ First element without checking if the vector is empty
unsafeHead :: G.Vector v a => v a -> a
unsafeHead = G.unsafeHead
{-# INLINE unsafeHead #-}

-- | /O(1)/ Last element without checking if the vector is empty
unsafeLast :: G.Vector v a => v a -> a
unsafeLast = G.unsafeLast
{-# INLINE unsafeLast #-}

-- Monadic indexing
-- ----------------

-- | /O(1)/ Indexing in a monad.
--
-- The monad allows operations to be strict in the vector when necessary.
-- Suppose vector copying is implemented like this:
--
-- > copy mv v = ... write mv i (v ! i) ...
--
-- For lazy vectors, @v ! i@ would not be evaluated which means that @mv@
-- would unnecessarily retain a reference to @v@ in each element written.
--
-- With 'indexM', copying can be implemented like this instead:
--
-- > copy mv v = ... do
-- >                   x <- indexM v i
-- >                   write mv i x
--
-- Here, no references to @v@ are retained because indexing (but /not/ the
-- elements) is evaluated eagerly.
--
indexM :: (Monad m, G.Vector v a) => v a -> Int -> m a
indexM = G.indexM
{-# INLINE indexM #-}

-- | /O(1)/ First element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
headM :: (Monad m, G.Vector v a) => v a -> m a
headM = G.headM
{-# INLINE headM #-}

-- | /O(1)/ Last element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
lastM :: (Monad m, G.Vector v a) => v a -> m a
lastM = G.lastM
{-# INLINE lastM #-}

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
-- explanation of why this is useful.
unsafeIndexM :: (Monad m, G.Vector v a) => v a -> Int -> m a
unsafeIndexM = G.unsafeIndexM
{-# INLINE unsafeIndexM #-}

-- | /O(1)/ First element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeHeadM :: (Monad m, G.Vector v a) => v a -> m a
unsafeHeadM = G.unsafeHeadM
{-# INLINE unsafeHeadM #-}

-- | /O(1)/ Last element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeLastM :: (Monad m, G.Vector v a) => v a -> m a
unsafeLastM = G.unsafeLastM
{-# INLINE unsafeLastM #-}

-- Extracting subvectors (slicing)
-- -------------------------------

-- | /O(1)/ Yield a slice of the vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Mixed u v a => Int   -- ^ @i@ starting index
                 -> Int   -- ^ @n@ length
                 -> v a
                 -> Vector a
slice i j m = mix (G.slice i j m)
{-# INLINE slice #-}

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty.
init :: Mixed u v a => v a -> Vector a
init m = mix (G.init m)
{-# INLINE init #-}

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty.
tail :: Mixed u v a => v a -> Vector a
tail m = mix (G.tail m)
{-# INLINE tail #-}

-- | /O(1)/ Yield at the first @n@ elements without copying. The vector may
-- contain less than @n@ elements in which case it is returned unchanged.
take :: Mixed u v a => Int -> v a -> Vector a
take i m = mix (G.take i m)
{-# INLINE take #-}

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector may
-- contain less than @n@ elements in which case an empty vector is returned.
drop :: Mixed u v a => Int -> v a -> Vector a
drop i m = mix (G.drop i m)
{-# INLINE drop #-}

-- | /O(1)/ Yield the first @n@ elements paired with the remainder without copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@
-- but slightly more efficient.
splitAt :: Mixed u v a => Int -> v a -> (Vector a, Vector a)
splitAt i m = case G.splitAt i m of
  (xs, ys) -> (mix xs, mix ys)
{-# INLINE splitAt #-}

-- | /O(1)/ Yield a slice of the vector without copying. The vector must
-- contain at least @i+n@ elements but this is not checked.
unsafeSlice :: Mixed u v a => Int   -- ^ @i@ starting index
                       -> Int   -- ^ @n@ length
                       -> v a
                       -> Vector a
unsafeSlice i j m = mix (G.unsafeSlice i j m)
{-# INLINE unsafeSlice #-}

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty but this is not checked.
unsafeInit :: Mixed u v a => v a -> Vector a
unsafeInit m = mix (G.unsafeInit m)
{-# INLINE unsafeInit #-}

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty but this is not checked.
unsafeTail :: Mixed u v a => v a -> Vector a
unsafeTail m = mix (G.unsafeTail m)
{-# INLINE unsafeTail #-}

-- | /O(1)/ Yield the first @n@ elements without copying. The vector must
-- contain at least @n@ elements but this is not checked.
unsafeTake :: Mixed u v a => Int -> v a -> Vector a
unsafeTake i m = mix (G.unsafeTake i m)
{-# INLINE unsafeTake #-}

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector
-- must contain at least @n@ elements but this is not checked.
unsafeDrop :: Mixed u v a => Int -> v a -> Vector a
unsafeDrop i m = mix (G.unsafeDrop i m)
{-# INLINE unsafeDrop #-}

-- Initialisation
-- --------------

-- | /O(1)/ Empty vector
empty :: Vector a
empty = G.empty
{-# INLINE empty #-}

-- | /O(1)/ Vector with exactly one element
singleton :: a -> Vector a
singleton = G.singleton
{-# INLINE singleton #-}

-- | /O(n)/ Vector of the given length with the same value in each position
replicate :: Int -> a -> Vector a
replicate = G.replicate
{-# INLINE replicate #-}

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index
generate :: Int -> (Int -> a) -> Vector a
generate = G.generate
{-# INLINE generate #-}

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
iterateN :: Int -> (a -> a) -> a -> Vector a
iterateN = G.iterateN
{-# INLINE iterateN #-}

-- Unfolding
-- ---------

-- | /O(n)/ Construct a vector by repeatedly applying the generator function
-- to a seed. The generator function yields 'Just' the next element and the
-- new seed or 'Nothing' if there are no more elements.
--
-- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- >  = <10,9,8,7,6,5,4,3,2,1>
unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
unfoldr = G.unfoldr
{-# INLINE unfoldr #-}

-- | /O(n)/ Construct a vector with at most @n@ by repeatedly applying the
-- generator function to the a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> Vector a
unfoldrN = G.unfoldrN
{-# INLINE unfoldrN #-}

-- | /O(n)/ Construct a vector with @n@ elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- > constructN 3 f = let a = f <> ; b = f <a> ; c = f <a,b> in f <a,b,c>
--
constructN :: Int -> (Vector a -> a) -> Vector a
constructN = G.constructN
{-# INLINE constructN #-}

-- | /O(n)/ Construct a vector with @n@ elements from right to left by
-- repeatedly applying the generator function to the already constructed part
-- of the vector.
--
-- > constructrN 3 f = let a = f <> ; b = f<a> ; c = f <b,a> in f <c,b,a>
--
constructrN :: Int -> (Vector a -> a) -> Vector a
constructrN = G.constructrN
{-# INLINE constructrN #-}

-- Enumeration
-- -----------

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
enumFromN :: Num a => a -> Int -> Vector a
enumFromN = G.enumFromN
{-# INLINE enumFromN #-}

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 0.1 5 = <1,1.1,1.2,1.3,1.4>
enumFromStepN :: Num a => a -> a -> Int -> Vector a
enumFromStepN = G.enumFromStepN
{-# INLINE enumFromStepN #-}

-- | /O(n)/ Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromN' instead.
enumFromTo :: Enum a => a -> a -> Vector a
enumFromTo = G.enumFromTo
{-# INLINE enumFromTo #-}

-- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: Enum a => a -> a -> a -> Vector a
enumFromThenTo = G.enumFromThenTo
{-# INLINE enumFromThenTo #-}

-- Concatenation
-- -------------

-- | /O(n)/ Prepend an element
cons :: Mixed u v a => a -> v a -> Vector a
cons a as = mix (G.cons a as)
{-# INLINE cons #-}

-- | /O(n)/ Append an element
snoc :: Mixed u v a => v a -> a -> Vector a
snoc as a = mix (G.snoc as a)
{-# INLINE snoc #-}

infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors
(++) :: (Mixed u v a, Mixed u' v' a) => v a -> v' a -> Vector a
m ++ n = mix m G.++ mix n
{-# INLINE (++) #-}

-- | /O(n)/ Concatenate all vectors in the list
concat :: Mixed u v a => [v a] -> Vector a
concat xs = mix (G.concat xs)
{-# INLINE concat #-}

-- Monadic initialisation
-- ----------------------

-- | /O(n)/ Execute the monadic action the given number of times and store the
-- results in a vector.
replicateM :: Monad m => Int -> m a -> m (Vector a)
replicateM = G.replicateM
{-# INLINE replicateM #-}

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index
generateM :: Monad m => Int -> (Int -> m a) -> m (Vector a)
generateM = G.generateM
{-# INLINE generateM #-}

-- | Execute the monadic action and freeze the resulting vector.
--
-- @
-- create (do { v \<- new 2; write v 0 \'a\'; write v 1 \'b\'; return v }) = \<'a','b'\>
-- @
create :: Mixed u v a => (forall s. ST s (u s a)) -> Vector a
-- NOTE: eta-expanded due to http://hackage.haskell.org/trac/ghc/ticket/4120
create p = mix (G.create p)
{-# INLINE create #-}

-- Restricting memory usage
-- ------------------------

-- | /O(n)/ Yield the argument but force it not to retain any extra memory,
-- possibly by copying it.
--
-- This is especially useful when dealing with slices. For example:
--
-- > force (slice 0 2 <huge vector>)
--
-- Here, the slice retains a reference to the huge vector. Forcing it creates
-- a copy of just the elements that belong to the slice and allows the huge
-- vector to be garbage collected.
force :: Mixed u v a => v a -> Vector a
force m = mix (G.force m)
{-# INLINE force #-}

-- Bulk updates
-- ------------

-- | /O(m+n)/ For each pair @(i,a)@ from the list, replace the vector
-- element at position @i@ by @a@.
--
-- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
--
(//) :: Mixed u v a => v a   -- ^ initial vector (of length @m@)
                -> [(Int, a)] -- ^ list of index/value pairs (of length @n@)
                -> Vector a
m // xs = mix (m G.// xs)
{-# INLINE (//) #-}

update_stream :: G.Vector v a => v a -> Stream (Int,a) -> v a
update_stream = modifyWithStream GM.update
{-# INLINE update_stream #-}

-- | /O(m+n)/ For each pair @(i,a)@ from the vector of index/value pairs,
-- replace the vector element at position @i@ by @a@.
--
-- > update <5,9,2,7> <(2,1),(0,3),(2,8)> = <3,9,8,7>
--
update :: (Mixed u v a, G.Vector v' (Int, a)) => v a -- ^ initial vector (of length @m@)
       -> v' (Int, a) -- ^ vector of index/value pairs (of length @n@)
       -> Vector a
update v w = mix (update_stream v (G.stream w))
{-# INLINE update #-}

-- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- corresponding value @a@ from the value vector, replace the element of the
-- initial vector at position @i@ by @a@.
--
-- > update_ <5,9,2,7>  <2,0,2> <1,3,8> = <3,9,8,7>
--
-- The function 'update' provides the same functionality and is usually more
-- convenient.
--
-- @
-- update_ xs is ys = 'update' xs ('zip' is ys)
-- @
update_ ::
  ( Mixed u v a, G.Vector v' Int, G.Vector v'' a
  ) => v a   -- ^ initial vector (of length @m@)
    -> v' Int -- ^ index vector (of length @n1@)
    -> v'' a   -- ^ value vector (of length @n2@)
    -> Vector a
update_ v is w = mix (update_stream v (Stream.zipWith (,) (G.stream is) (G.stream w)))
{-# INLINE update_ #-}

-- | Same as ('//') but without bounds checking.
unsafeUpd :: Mixed u v a => v a -> [(Int, a)] -> Vector a
unsafeUpd v us = mix (unsafeUpdate_stream v (Stream.fromList us))
{-# INLINE unsafeUpd #-}

unsafeUpdate_stream :: G.Vector v a => v a -> Stream (Int,a) -> v a
unsafeUpdate_stream = modifyWithStream GM.unsafeUpdate
{-# INLINE unsafeUpdate_stream #-}

-- | Same as 'update' but without bounds checking.
unsafeUpdate :: (Mixed u v a, G.Vector v' (Int, a)) => v a -> v' (Int, a) -> Vector a
unsafeUpdate v w = mix (unsafeUpdate_stream v (G.stream w))
{-# INLINE unsafeUpdate #-}

-- | Same as 'update_' but without bounds checking.
unsafeUpdate_ :: ( Mixed u v a, G.Vector v' Int, G.Vector v'' a
  ) => v a -> v' Int -> v'' a -> Vector a
unsafeUpdate_ v is w = mix (unsafeUpdate_stream v (Stream.zipWith (,) (G.stream is) (G.stream w)))
{-# INLINE unsafeUpdate_ #-}

-- Accumulations
-- -------------

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the vector element
-- @a@ at position @i@ by @f a b@.
--
-- > accum (+) <5,9,2> [(2,4),(1,6),(0,3),(1,7)] = <5+3, 9+6+7, 2+4>
accum :: Mixed u v a => (a -> b -> a) -- ^ accumulating function @f@
      -> v a      -- ^ initial vector (of length @m@)
      -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
      -> Vector a
accum f v us = mix (accum_stream f v (Stream.fromList us))
{-# INLINE accum #-}

-- | /O(m+n)/ For each pair @(i,b)@ from the vector of pairs, replace the vector
-- element @a@ at position @i@ by @f a b@.
--
-- > accumulate (+) <5,9,2> <(2,4),(1,6),(0,3),(1,7)> = <5+3, 9+6+7, 2+4>
accumulate :: (Mixed u v a, G.Vector v' (Int, b))
           => (a -> b -> a)  -- ^ accumulating function @f@
           -> v a       -- ^ initial vector (of length @m@)
           -> v' (Int,b) -- ^ vector of index/value pairs (of length @n@)
           -> Vector a
accumulate f v us = mix (accum_stream f v (G.stream us))
{-# INLINE accumulate #-}

-- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- corresponding value @b@ from the the value vector,
-- replace the element of the initial vector at
-- position @i@ by @f a b@.
--
-- > accumulate_ (+) <5,9,2> <2,1,0,1> <4,6,3,7> = <5+3, 9+6+7, 2+4>
--
-- The function 'accumulate' provides the same functionality and is usually more
-- convenient.
--
-- @
-- accumulate_ f as is bs = 'accumulate' f as ('zip' is bs)
-- @
accumulate_
  :: (Mixed u v a, G.Vector v' Int, G.Vector v'' b)
  => (a -> b -> a) -- ^ accumulating function @f@
  -> v a      -- ^ initial vector (of length @m@)
  -> v' Int    -- ^ index vector (of length @n1@)
  -> v'' b      -- ^ value vector (of length @n2@)
  -> Vector a
accumulate_ f v is xs = mix (accum_stream f v (Stream.zipWith (,) (G.stream is) (G.stream xs)))
{-# INLINE accumulate_ #-}

accum_stream :: G.Vector v a => (a -> b -> a) -> v a -> Stream (Int,b) -> v a
accum_stream f = modifyWithStream (GM.accum f)
{-# INLINE accum_stream #-}

-- | Same as 'accum' but without bounds checking.
unsafeAccum :: Mixed u v a => (a -> b -> a) -> v a -> [(Int,b)] -> Vector a
unsafeAccum f v us = mix (unsafeAccum_stream f v (Stream.fromList us))

{-# INLINE unsafeAccum #-}

-- | Same as 'accumulate' but without bounds checking.
unsafeAccumulate :: (Mixed u v a, G.Vector v' (Int, b)) => (a -> b -> a) -> v a -> v' (Int,b) -> Vector a
unsafeAccumulate f v us = mix (unsafeAccum_stream f v (G.stream us))
{-# INLINE unsafeAccumulate #-}

-- | Same as 'accumulate_' but without bounds checking.
unsafeAccumulate_
  :: (Mixed u v a, G.Vector v' Int, G.Vector v'' b)
  => (a -> b -> a) -> v a -> v' Int -> v'' b -> Vector a
unsafeAccumulate_ f v is xs = mix (unsafeAccum_stream f v (Stream.zipWith (,) (G.stream is) (G.stream xs)))
{-# INLINE unsafeAccumulate_ #-}

unsafeAccum_stream :: G.Vector v a => (a -> b -> a) -> v a -> Stream (Int,b) -> v a
unsafeAccum_stream f = modifyWithStream (GM.unsafeAccum f)
{-# INLINE unsafeAccum_stream #-}

-- Permutations
-- ------------

-- | /O(n)/ Reverse a vector
reverse :: Mixed u v a => v a -> Vector a
reverse m = mix (G.reverse m)
{-# INLINE reverse #-}

-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @xs'!'i@. This is equivalent to @'map' (xs'!') is@ but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute :: (Mixed u v a, G.Vector v' Int) => v a -> v' Int -> Vector a
-- backpermute m n = G.backpermute (mix m) (mix n)
-- {-# INLINE backpermute #-}

-- This somewhat non-intuitive definition ensures that the resulting vector
-- does not retain references to the original one even if it is lazy in its
-- elements. This would not be the case if we simply used map (v!)
backpermute v is = mix
                 $ (`asTypeOf` v)
                 $ seq v
                 $ seq n
                 $ G.unstream
                 $ Stream.unbox
                 $ Stream.map index
                 $ G.stream is
  where
    n = length v

    {-# INLINE index #-}
    -- NOTE: we do it this way to avoid triggering LiberateCase on n in
    -- polymorphic code
    index i = BOUNDS_CHECK(checkIndex) "backpermute" i n
            $ G.basicUnsafeIndexM v i

-- | Same as 'backpermute' but without bounds checking.
unsafeBackpermute :: (Mixed u v a, G.Vector v' Int) => v a -> v' Int -> Vector a
unsafeBackpermute v is = mix
                       $ (`asTypeOf` v)
                       $ seq v
                       $ seq n
                       $ G.unstream
                       $ Stream.unbox
                       $ Stream.map index
                       $ G.stream is
  where
    n = length v

    {-# INLINE index #-}
    -- NOTE: we do it this way to avoid triggering LiberateCase on n in
    -- polymorphic code
    index i = UNSAFE_CHECK(checkIndex) "unsafeBackpermute" i n
            $ G.basicUnsafeIndexM v i

{-# INLINE unsafeBackpermute #-}

-- Safe destructive updates
-- ------------------------

-- | Apply a destructive operation to a vector. The operation will be
-- performed in place if it is safe to do so and will modify a copy of the
-- vector otherwise.
--
-- @
-- modify (\\v -> write v 0 \'x\') ('replicate' 3 \'a\') = \<\'x\',\'a\',\'a\'\>
-- @
modify :: Mixed u v a => (forall s. u s a -> ST s ()) -> v a -> Vector a
modify p v = mix (G.modify p v)
{-# INLINE modify #-}

-- Indexing
-- --------

-- | /O(n)/ Pair each element in a vector with its index
indexed :: (G.Vector v a, Mixed u v (Int, a)) => v a -> Vector (Int,a)
indexed m = mix (G.indexed m)
{-# INLINE indexed #-}

-- Mapping
-- -------

-- | /O(n)/ Map a function over a vector
map :: G.Vector v a => (a -> b) -> v a -> Vector b
map f = boxed . G.unstream . Stream.inplace (MStream.map f) . G.stream


{-# INLINE map #-}

-- | /O(n)/ Apply a function to every element of a vector and its index
imap :: G.Vector v a => (Int -> a -> b) -> v a -> Vector b
-- imap f m = mix (G.imap f m)
imap f = boxed . G.unstream . Stream.inplace (MStream.map (uncurry f) . MStream.indexed) . G.stream
{-# INLINE imap #-}

-- | Map a function over a vector and concatenate the results.
concatMap :: (Mixed u v b, G.Vector v' a) => (a -> v b) -> v' a -> Vector b
concatMap f = mix . G.concat . Stream.toList . Stream.map f . G.stream
{-# INLINE concatMap #-}

-- Monadic mapping
-- ---------------

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results
mapM :: (Monad m, G.Vector v a) => (a -> m b) -> v a -> m (Vector b)
mapM f = unstreamM . Stream.mapM f . G.stream
{-# INLINE mapM #-}

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results
mapM_ :: (Monad m, G.Vector v a) => (a -> m b) -> v a -> m ()
mapM_ f = Stream.mapM_ f . G.stream
{-# INLINE mapM_ #-}

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results. Equvalent to @flip 'mapM'@.
forM :: (Monad m, G.Vector v a) => v a -> (a -> m b) -> m (Vector b)
forM as f = mapM f as
{-# INLINE forM #-}

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: (Monad m, G.Vector v a) => v a -> (a -> m b) -> m ()
forM_ as f = mapM_ f as
{-# INLINE forM_ #-}

-- Zipping
-- -------

-- | /O(min(m,n))/ Zip two vectors with the given function.
zipWith :: (G.Vector va a, G.Vector vb b)
        => (a -> b -> c) -> va a -> vb b -> Vector c
zipWith k a b = boxed (G.unstream (Stream.zipWith k (G.stream a) (G.stream b)))
{-# INLINE zipWith #-}

-- | Zip three vectors with the given function.
zipWith3 :: (G.Vector va a, G.Vector vb b, G.Vector vc c)
         => (a -> b -> c -> d) -> va a -> vb b -> vc c -> Vector d
zipWith3 k a b c = boxed (G.unstream (Stream.zipWith3 k (G.stream a) (G.stream b) (G.stream c)))
{-# INLINE zipWith3 #-}

zipWith4 :: (G.Vector va a, G.Vector vb b, G.Vector vc c, G.Vector vd d)
         => (a -> b -> c -> d -> e) -> va a -> vb b -> vc c -> vd d -> Vector e
zipWith4 k a b c d = boxed (G.unstream (Stream.zipWith4 k (G.stream a) (G.stream b) (G.stream c) (G.stream d)))
{-# INLINE zipWith4 #-}

zipWith5 :: (G.Vector va a, G.Vector vb b, G.Vector vc c, G.Vector vd d, G.Vector ve e)
         => (a -> b -> c -> d -> e -> f) -> va a -> vb b -> vc c -> vd d -> ve e -> Vector f
zipWith5 k a b c d e = boxed (G.unstream (Stream.zipWith5 k (G.stream a) (G.stream b) (G.stream c) (G.stream d) (G.stream e)))
{-# INLINE zipWith5 #-}

zipWith6 :: (G.Vector va a, G.Vector vb b, G.Vector vc c, G.Vector vd d, G.Vector ve e, G.Vector vf f)
         => (a -> b -> c -> d -> e -> f -> g) -> va a -> vb b -> vc c -> vd d -> ve e -> vf f -> Vector g
zipWith6 k a b c d e f = boxed (G.unstream (Stream.zipWith6 k (G.stream a) (G.stream b) (G.stream c) (G.stream d) (G.stream e) (G.stream f)))
{-# INLINE zipWith6 #-}


-- | /O(min(m,n))/ Zip two vectors with a function that also takes the
-- elements' indices.

izipWith :: (G.Vector va a, G.Vector vb b)
        => (Int -> a -> b -> c) -> va a -> vb b -> Vector c
izipWith f xs ys = boxed $ G.unstream $
   Stream.zipWith (uncurry f) (Stream.indexed (G.stream xs)) (G.stream ys)

{-# INLINE izipWith #-}

-- | Zip three vectors and their indices with the given function.
izipWith3 :: (G.Vector va a, G.Vector vb b, G.Vector vc c)
         => (Int -> a -> b -> c -> d) -> va a -> vb b -> vc c -> Vector d
izipWith3 f xs ys zs = boxed $ G.unstream $
   Stream.zipWith3 (uncurry f) (Stream.indexed (G.stream xs)) (G.stream ys) (G.stream zs)
{-# INLINE izipWith3 #-}

izipWith4 :: (G.Vector va a, G.Vector vb b, G.Vector vc c, G.Vector vd d)
         => (Int -> a -> b -> c -> d -> e) -> va a -> vb b -> vc c -> vd d -> Vector e
izipWith4 f xs ys zs ws = boxed $ G.unstream $
   Stream.zipWith4 (uncurry f) (Stream.indexed (G.stream xs)) (G.stream ys) (G.stream zs) (G.stream ws)
{-# INLINE izipWith4 #-}

izipWith5 :: (G.Vector va a, G.Vector vb b, G.Vector vc c, G.Vector vd d, G.Vector ve e)
         => (Int -> a -> b -> c -> d -> e -> f) -> va a -> vb b -> vc c -> vd d -> ve e -> Vector f
izipWith5 k a b c d e = boxed (G.unstream (Stream.zipWith5 (uncurry k) (Stream.indexed (G.stream a)) (G.stream b) (G.stream c) (G.stream d) (G.stream e)))
{-# INLINE izipWith5 #-}

izipWith6 :: (G.Vector va a, G.Vector vb b, G.Vector vc c, G.Vector vd d, G.Vector ve e, G.Vector vf f)
         => (Int -> a -> b -> c -> d -> e -> f -> g) -> va a -> vb b -> vc c -> vd d -> ve e -> vf f -> Vector g
izipWith6 k a b c d e f = boxed (G.unstream (Stream.zipWith6 (uncurry k) (Stream.indexed (G.stream a)) (G.stream b) (G.stream c) (G.stream d) (G.stream e) (G.stream f)))
{-# INLINE izipWith6 #-}

-- | Elementwise pairing of array elements.
zip :: (G.Vector va a, G.Vector vb b)
    => va a -> vb b -> Vector (a, b)
-- zip a b = mix (H.V a b) -- we would need to trim appropriately, and this would likely interfere with streaming. TODO: fix up and benchmark?
zip = zipWith (,)
{-# INLINE zip #-}

-- | zip together three vectors into a vector of triples
zip3 :: (G.Vector va a, G.Vector vb b, G.Vector vc c)
     => va  a -> vb b -> vc c -> Vector (a, b, c)
zip3 = zipWith3 (,,)
{-# INLINE zip3 #-}

zip4 :: (G.Vector va a, G.Vector vb b, G.Vector vc c, G.Vector vd d)
     => va a -> vb b -> vc c -> vd d -> Vector (a, b, c, d)
zip4 = zipWith4 (,,,)
{-# INLINE zip4 #-}

zip5 :: (G.Vector va a, G.Vector vb b, G.Vector vc c, G.Vector vd d, G.Vector ve e)
     => va a -> vb b -> vc c -> vd d -> ve e -> Vector (a, b, c, d, e)
zip5 = zipWith5 (,,,,)
{-# INLINE zip5 #-}

zip6 :: (G.Vector va a, G.Vector vb b, G.Vector vc c, G.Vector vd d, G.Vector ve e, G.Vector vf f)
     => va a -> vb b -> vc c -> vd d -> ve e -> vf f -> Vector (a, b, c, d, e, f)
zip6 = zipWith6 (,,,,,)
{-# INLINE zip6 #-}

-- Unzipping
-- ---------

-- | /O(min(m,n))/ Unzip a vector of pairs.
unzip :: G.Vector v (a, b) => v (a, b) -> (Vector a, Vector b)
unzip v = (map fst v, map snd v)
{-# INLINE unzip #-}

unzip3 :: G.Vector v (a, b, c) => v (a, b, c) -> (Vector a, Vector b, Vector c)
unzip3 xs = (map (\(a, _, _) -> a) xs,
             map (\(_, b, _) -> b) xs,
             map (\(_, _, c) -> c) xs)
{-# INLINE unzip3 #-}

unzip4 :: G.Vector v (a, b, c, d) => v (a, b, c, d) -> (Vector a, Vector b, Vector c, Vector d)
unzip4 xs = (map (\(a, _, _, _) -> a) xs,
             map (\(_, b, _, _) -> b) xs,
             map (\(_, _, c, _) -> c) xs,
             map (\(_, _, _, d) -> d) xs)
{-# INLINE unzip4 #-}

unzip5 :: G.Vector v (a, b, c, d, e) => v (a, b, c, d, e) -> (Vector a, Vector b, Vector c, Vector d, Vector e)
unzip5 xs = (map (\(a, _, _, _, _) -> a) xs,
             map (\(_, b, _, _, _) -> b) xs,
             map (\(_, _, c, _, _) -> c) xs,
             map (\(_, _, _, d, _) -> d) xs,
             map (\(_, _, _, _, e) -> e) xs)
{-# INLINE unzip5 #-}

unzip6 :: G.Vector v (a, b, c, d, e, f) => v (a, b, c, d, e, f) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f)
unzip6 xs = (map (\(a, _, _, _, _, _) -> a) xs,
             map (\(_, b, _, _, _, _) -> b) xs,
             map (\(_, _, c, _, _, _) -> c) xs,
             map (\(_, _, _, d, _, _) -> d) xs,
             map (\(_, _, _, _, e, _) -> e) xs,
             map (\(_, _, _, _, _, f) -> f) xs)
{-# INLINE unzip6 #-}

-- Monadic zipping
-- ---------------

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and yield a
-- vector of results
zipWithM :: (Monad m, G.Vector va a, G.Vector vb b) => (a -> b -> m c) -> va a -> vb b -> m (Vector c)
zipWithM f as bs = unstreamM $ Stream.zipWithM f (G.stream as) (G.stream bs)
{-# INLINE zipWithM #-}


-- | /O(min(m,n))/ Zip the two vectors with the monadic action and ignore the
-- results
zipWithM_ :: (Monad m, G.Vector va a, G.Vector vb b) => (a -> b -> m c) -> va a -> vb b -> m ()
zipWithM_ f as bs = Stream.zipWithM_ f (G.stream as) (G.stream bs)
{-# INLINE zipWithM_ #-}

-- Filtering
-- ---------

-- | /O(n)/ Drop elements that do not satisfy the predicate
filter :: Mixed u v a => (a -> Bool) -> v a -> Vector a
{-# INLINE filter #-}
filter f = mix . G.filter f

-- | /O(n)/ Drop elements that do not satisfy the predicate which is applied to
-- values and their indices
ifilter :: Mixed u v a => (Int -> a -> Bool) -> v a -> Vector a
ifilter f = mix . G.ifilter f
{-# INLINE ifilter #-}

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate
filterM :: (Monad m, Mixed u v a) => (a -> m Bool) -> v a -> m (Vector a)
filterM f = liftM mix . G.filterM f
{-# INLINE filterM #-}

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate
-- without copying.
takeWhile :: Mixed u v a => (a -> Bool) -> v a -> Vector a
takeWhile f = mix . G.takeWhile f
{-# INLINE takeWhile #-}

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- without copying.
dropWhile :: Mixed u v a => (a -> Bool) -> v a -> Vector a
dropWhile f = mix . G.dropWhile f
{-# INLINE dropWhile #-}

-- Parititioning
-- -------------

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a sometimes
-- reduced performance compared to 'unstablePartition'.
partition :: Mixed u v a => (a -> Bool) -> v a -> (Vector a, Vector a)
partition f as = case G.partition f as of
  (l,r) -> (mix l, mix r)
{-# INLINE partition #-}

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't.
-- The order of the elements is not preserved but the operation is often
-- faster than 'partition'.
unstablePartition :: Mixed u v a => (a -> Bool) -> v a -> (Vector a, Vector a)
unstablePartition f as = case G.unstablePartition f as of
  (l,r) -> (mix l, mix r)
{-# INLINE unstablePartition #-}

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest without copying.
span :: Mixed u v a => (a -> Bool) -> v a -> (Vector a, Vector a)
span f as = case G.span f as of
  (l,r) -> (mix l, mix r)
{-# INLINE span #-}

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
break :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
break f as = case G.break f as of
  (l,r) -> (mix l, mix r)
{-# INLINE break #-}

-- Searching
-- ---------

infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element
elem :: (G.Vector v a, Eq a) => a -> v a -> Bool
elem = G.elem
{-# INLINE elem #-}

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem')
notElem :: (G.Vector v a, Eq a) => a -> v a -> Bool
notElem = G.notElem
{-# INLINE notElem #-}

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: (G.Vector v a) => (a -> Bool) -> v a -> Maybe a
find = G.find
{-# INLINE find #-}

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: G.Vector v a => (a -> Bool) -> v a -> Maybe Int
findIndex = G.findIndex
{-# INLINE findIndex #-}

-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
findIndices :: G.Vector v a => (a -> Bool) -> v a -> Vector Int
findIndices f = unboxed . G.unstream
              . Stream.inplace (MStream.map fst . MStream.filter (f . snd) . MStream.indexed)
              . G.stream
{-# INLINE findIndices #-}

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: (G.Vector v a, Eq a) => a -> v a -> Maybe Int
elemIndex = G.elemIndex
{-# INLINE elemIndex #-}

-- | /O(n)/ Yield the indices of all occurences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
elemIndices :: (G.Vector v a, Eq a) => a -> v a -> Vector Int
elemIndices x = findIndices (x==)
{-# INLINE elemIndices #-}

-- Folding
-- -------

-- | /O(n)/ Left fold
foldl :: G.Vector v b => (a -> b -> a) -> a -> v b -> a
foldl = G.foldl
{-# INLINE foldl #-}

-- | /O(n)/ Left fold on non-empty vectors
foldl1 :: G.Vector v a => (a -> a -> a) -> v a -> a
foldl1 = G.foldl1
{-# INLINE foldl1 #-}

-- | /O(n)/ Left fold with strict accumulator
foldl' :: G.Vector v b => (a -> b -> a) -> a -> v b -> a
foldl' = G.foldl'
{-# INLINE foldl' #-}

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator
foldl1' :: G.Vector v a => (a -> a -> a) -> v a -> a
foldl1' = G.foldl1'
{-# INLINE foldl1' #-}

-- | /O(n)/ Right fold
foldr :: G.Vector v a => (a -> b -> b) -> b -> v a -> b
foldr = G.foldr
{-# INLINE foldr #-}

-- | /O(n)/ Right fold on non-empty vectors
foldr1 :: G.Vector v a => (a -> a -> a) -> v a -> a
foldr1 = G.foldr1
{-# INLINE foldr1 #-}

-- | /O(n)/ Right fold with a strict accumulator
foldr' :: G.Vector v a => (a -> b -> b) -> b -> v a -> b
foldr' = G.foldr'
{-# INLINE foldr' #-}

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator
foldr1' :: G.Vector v a => (a -> a -> a) -> v a -> a
foldr1' = G.foldr1'
{-# INLINE foldr1' #-}

-- | /O(n)/ Left fold (function applied to each element and its index)
ifoldl :: G.Vector v b => (a -> Int -> b -> a) -> a -> v b -> a
ifoldl = G.ifoldl
{-# INLINE ifoldl #-}

-- | /O(n)/ Left fold with strict accumulator (function applied to each element
-- and its index)
ifoldl' :: G.Vector v b => (a -> Int -> b -> a) -> a -> v b -> a
ifoldl' = G.ifoldl'
{-# INLINE ifoldl' #-}

-- | /O(n)/ Right fold (function applied to each element and its index)
ifoldr :: G.Vector v a => (Int -> a -> b -> b) -> b -> v a -> b
ifoldr = G.ifoldr
{-# INLINE ifoldr #-}

-- | /O(n)/ Right fold with strict accumulator (function applied to each
-- element and its index)
ifoldr' :: G.Vector v a => (Int -> a -> b -> b) -> b -> v a -> b
ifoldr' = G.ifoldr'
{-# INLINE ifoldr' #-}

-- Specialised folds
-- -----------------

-- | /O(n)/ Check if all elements satisfy the predicate.
all :: G.Vector v a => (a -> Bool) -> v a -> Bool
all = G.all
{-# INLINE all #-}

-- | /O(n)/ Check if any element satisfies the predicate.
any :: G.Vector v a => (a -> Bool) -> v a -> Bool
{-# INLINE any #-}
any = G.any

-- | /O(n)/ Check if all elements are 'True'
and :: G.Vector v Bool => v Bool -> Bool
and = G.and
{-# INLINE and #-}

-- | /O(n)/ Check if any element is 'True'
or :: G.Vector v Bool => v Bool -> Bool
{-# INLINE or #-}
or = G.or

-- | /O(n)/ Compute the sum of the elements
sum :: (G.Vector v a, Num a) => v a -> a
sum = G.sum
{-# INLINE sum #-}

-- | /O(n)/ Compute the produce of the elements
product :: (G.Vector v a, Num a) => v a -> a
product = G.product
{-# INLINE product #-}

-- | /O(n)/ Yield the maximum element of the vector. The vector may not be
-- empty.
maximum :: (G.Vector v a, Ord a) => v a -> a
maximum = G.maximum
{-# INLINE maximum #-}

-- | /O(n)/ Yield the maximum element of the vector according to the given
-- comparison function. The vector may not be empty.
maximumBy :: G.Vector v a => (a -> a -> Ordering) -> v a -> a
maximumBy = G.maximumBy
{-# INLINE maximumBy #-}

-- | /O(n)/ Yield the minimum element of the vector. The vector may not be
-- empty.
minimum :: (G.Vector v a, Ord a) => v a -> a
minimum = G.minimum
{-# INLINE minimum #-}

-- | /O(n)/ Yield the minimum element of the vector according to the given
-- comparison function. The vector may not be empty.
minimumBy :: G.Vector v a => (a -> a -> Ordering) -> v a -> a
minimumBy = G.minimumBy
{-# INLINE minimumBy #-}

-- | /O(n)/ Yield the index of the maximum element of the vector. The vector
-- may not be empty.
maxIndex :: (G.Vector v a, Ord a) => v a -> Int
maxIndex = G.maxIndex
{-# INLINE maxIndex #-}

-- | /O(n)/ Yield the index of the maximum element of the vector according to
-- the given comparison function. The vector may not be empty.
maxIndexBy :: G.Vector v a => (a -> a -> Ordering) -> v a -> Int
maxIndexBy = G.maxIndexBy
{-# INLINE maxIndexBy #-}

-- | /O(n)/ Yield the index of the minimum element of the vector. The vector
-- may not be empty.
minIndex :: (G.Vector v a, Ord a) => v a -> Int
minIndex = G.minIndex
{-# INLINE minIndex #-}

-- | /O(n)/ Yield the index of the minimum element of the vector according to
-- the given comparison function. The vector may not be empty.
minIndexBy :: G.Vector v a => (a -> a -> Ordering) -> v a -> Int
minIndexBy = G.minIndexBy
{-# INLINE minIndexBy #-}

-- Monadic folds
-- -------------

-- | /O(n)/ Monadic fold
foldM :: (G.Vector v b, Monad m) => (a -> b -> m a) -> a -> v b -> m a
foldM = G.foldM
{-# INLINE foldM #-}

-- | /O(n)/ Monadic fold over non-empty vectors
fold1M :: (G.Vector v a, Monad m) => (a -> a -> m a) -> v a -> m a
fold1M = G.fold1M
{-# INLINE fold1M #-}

-- | /O(n)/ Monadic fold with strict accumulator
foldM' :: (G.Vector v b, Monad m) => (a -> b -> m a) -> a -> v b -> m a
foldM' = G.foldM'
{-# INLINE foldM' #-}

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
fold1M' :: (G.Vector v a, Monad m) => (a -> a -> m a) -> v a -> m a
fold1M' = G.fold1M'
{-# INLINE fold1M' #-}

-- | /O(n)/ Monadic fold that discards the result
foldM_ :: (G.Vector v b, Monad m) => (a -> b -> m a) -> a -> v b -> m ()
foldM_ = G.foldM_
{-# INLINE foldM_ #-}

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result
fold1M_ :: (G.Vector v a, Monad m) => (a -> a -> m a) -> v a -> m ()
fold1M_ = G.fold1M_
{-# INLINE fold1M_ #-}

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
foldM'_ :: (G.Vector v b, Monad m) => (a -> b -> m a) -> a -> v b -> m ()
foldM'_ = G.foldM'_
{-# INLINE foldM'_ #-}

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
-- that discards the result
fold1M'_ :: (G.Vector v a, Monad m) => (a -> a -> m a) -> v a -> m ()
fold1M'_ = G.fold1M'_
{-# INLINE fold1M'_ #-}

-- Monadic sequencing
-- ------------------

-- | Evaluate each action and collect the results
sequence :: (Mixed u v (m a), Monad m) => v (m a) -> m (Vector a)
sequence = mapM id
{-# INLINE sequence #-}

-- | Evaluate each action and discard the results
sequence_ :: (G.Vector v (m a), Monad m) => v (m a) -> m ()
sequence_ = mapM_ id
{-# INLINE sequence_ #-}

-- Prefix sums (scans)
-- -------------------

-- | /O(n)/ Prescan
--
-- @
-- prescanl f z = 'init' . 'scanl' f z
-- @
--
-- Example: @prescanl (+) 0 \<1,2,3,4\> = \<0,1,3,6\>@
--
prescanl :: G.Vector v b => (a -> b -> a) -> a -> v b -> Vector a
prescanl f z = boxed . G.unstream . Stream.inplace (MStream.prescanl f z) . G.stream
{-# INLINE prescanl #-}

-- | /O(n)/ Prescan with strict accumulator
prescanl' :: G.Vector v b => (a -> b -> a) -> a -> v b -> Vector a
prescanl' f z = boxed . G.unstream . Stream.inplace (MStream.prescanl' f z) . G.stream
{-# INLINE prescanl' #-}

-- | /O(n)/ Scan
--
-- @
-- postscanl f z = 'tail' . 'scanl' f z
-- @
--
-- Example: @postscanl (+) 0 \<1,2,3,4\> = \<1,3,6,10\>@
--
postscanl :: G.Vector v b => (a -> b -> a) -> a -> v b -> Vector a
postscanl f z = boxed . G.unstream . Stream.inplace (MStream.postscanl f z) . G.stream
{-# INLINE postscanl #-}

-- | /O(n)/ Scan with strict accumulator
postscanl' :: G.Vector v b => (a -> b -> a) -> a -> v b -> Vector a
postscanl' f z = boxed . G.unstream . Stream.inplace (MStream.postscanl' f z) . G.stream
{-# INLINE postscanl' #-}


-- | /O(n)/ Haskell-style scan
--
-- > scanl f z <x1,...,xn> = <y1,...,y(n+1)>
-- >   where y1 = z
-- >         yi = f y(i-1) x(i-1)
--
-- Example: @scanl (+) 0 \<1,2,3,4\> = \<0,1,3,6,10\>@
--

scanl :: G.Vector v b => (a -> b -> a) -> a -> v b -> Vector a
scanl f z = boxed . G.unstream . Stream.scanl f z . G.stream
{-# INLINE scanl #-}

-- | /O(n)/ Haskell-style scan with strict accumulator
scanl' :: G.Vector v b => (a -> b -> a) -> a -> v b -> Vector a
scanl' f z = boxed . G.unstream . Stream.scanl' f z . G.stream
{-# INLINE scanl' #-}

-- | /O(n)/ Scan over a non-empty vector
--
-- > scanl f <x1,...,xn> = <y1,...,yn>
-- >   where y1 = x1
-- >         yi = f y(i-1) xi
--
scanl1 :: Mixed u v a => (a -> a -> a) -> v a -> Vector a
scanl1 f = mix . G.scanl1 f
{-# INLINE scanl1 #-}

-- | /O(n)/ Scan over a non-empty vector with a strict accumulator
scanl1' :: Mixed u v a => (a -> a -> a) -> v a -> Vector a
scanl1' f = mix . G.scanl1' f
{-# INLINE scanl1' #-}

-- | /O(n)/ Right-to-left prescan
--
-- @
-- prescanr f z = 'reverse' . 'prescanl' (flip f) z . 'reverse'
-- @
--
prescanr :: G.Vector v a => (a -> b -> b) -> b -> v a -> Vector b
prescanr f z = boxed . G.unstreamR . Stream.inplace (MStream.prescanl (flip f) z) . G.streamR
{-# INLINE prescanr #-}

-- | /O(n)/ Right-to-left prescan with strict accumulator
prescanr' :: G.Vector v a => (a -> b -> b) -> b -> v a -> Vector b
{-# INLINE prescanr' #-}
prescanr' f z = boxed . G.unstreamR . Stream.inplace (MStream.prescanl' (flip f) z) . G.streamR

-- | /O(n)/ Right-to-left scan
postscanr :: G.Vector v a => (a -> b -> b) -> b -> v a -> Vector b
postscanr f z = boxed . G.unstreamR . Stream.inplace (MStream.postscanl (flip f) z) . G.streamR
{-# INLINE postscanr #-}

-- | /O(n)/ Right-to-left scan with strict accumulator
postscanr' :: G.Vector v a => (a -> b -> b) -> b -> v a -> Vector b
postscanr' f z = boxed . G.unstreamR . Stream.inplace (MStream.postscanl' (flip f) z) . G.streamR
{-# INLINE postscanr' #-}

-- | /O(n)/ Right-to-left Haskell-style scan
scanr :: G.Vector v a => (a -> b -> b) -> b -> v a -> Vector b
scanr f z = boxed . G.unstreamR . Stream.scanl (flip f) z . G.streamR
{-# INLINE scanr #-}


-- | /O(n)/ Right-to-left Haskell-style scan with strict accumulator
scanr' :: G.Vector v a => (a -> b -> b) -> b -> v a -> Vector b
scanr' f z = boxed . G.unstreamR . Stream.scanl' (flip f) z . G.streamR

{-# INLINE scanr' #-}

-- | /O(n)/ Right-to-left scan over a non-empty vector
scanr1 :: Mixed u v a => (a -> a -> a) -> v a -> Vector a
{-# INLINE scanr1 #-}
scanr1 f = mix . G.scanr1 f

-- | /O(n)/ Right-to-left scan over a non-empty vector with a strict
-- accumulator
scanr1' :: (a -> a -> a) -> Vector a -> Vector a
scanr1' f = mix . G.scanr1' f
{-# INLINE scanr1' #-}

-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert a vector to a list
toList :: G.Vector v a => v a -> [a]
toList = G.toList
{-# INLINE toList #-}

-- | /O(n)/ Convert a list to a vector
fromList :: [a] -> Vector a
fromList = boxed . G.fromList
{-# INLINE fromList #-}

-- | /O(n)/ Convert the first @n@ elements of a list to a vector
--
-- @
-- fromListN n xs = 'fromList' ('take' n xs)
-- @
fromListN :: Int -> [a] -> Vector a
fromListN n = boxed . G.fromListN n
{-# INLINE fromListN #-}

-- Conversions - Mutable vectors
-- -----------------------------

-- | /O(1)/ Unsafe convert a mutable vector to an immutable one without
-- copying. The mutable vector may not be used after this operation.
unsafeFreeze :: (PrimMonad m, Mixed u v a) => u (PrimState m) a -> m (Vector a)
unsafeFreeze = liftM mix . G.unsafeFreeze
{-# INLINE unsafeFreeze #-}

-- | /O(1)/ Unsafely convert an immutable vector to a mutable one without
-- copying. The immutable vector may not be used after this operation.
unsafeThaw :: (PrimMonad m, Mixed u v a) => v a -> m (MVector (PrimState m) a)
unsafeThaw = liftM mmix . G.unsafeThaw
{-# INLINE unsafeThaw #-}

-- | /O(n)/ Yield a mutable copy of the immutable vector.
thaw :: (PrimMonad m, Mixed u v a) => v a -> m (MVector (PrimState m) a)
thaw = liftM mmix . G.thaw
{-# INLINE thaw #-}

-- | /O(n)/ Yield an immutable copy of the mutable vector.
freeze :: (PrimMonad m, Mixed u v a) => u (PrimState m) a -> m (Vector a)
freeze = liftM mix . G.freeze
{-# INLINE freeze #-}

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length. This is not checked.
unsafeCopy :: (PrimMonad m, Mixed u v a, Mixed u' v' a) => u (PrimState m) a -> v' a -> m ()
unsafeCopy dst src = G.unsafeCopy (mmix dst) (mix src)
{-# INLINE unsafeCopy #-}

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length.
copy :: (PrimMonad m, Mixed u v a, Mixed u' v' a) => u (PrimState m) a -> v' a -> m ()
copy dst src = G.copy (mmix dst) (mix src)
{-# INLINE copy #-}

-- Utilities
-- ---------

unstreamM :: (Monad m, G.Vector v a) => MStream m a -> m (v a)
unstreamM s = do
  xs <- MStream.toList s
  return $ G.unstream $ Stream.unsafeFromList (MStream.size s) xs
{-# INLINE [1] unstreamM #-}

-- We have to make sure that this is strict in the stream but we can't seq on
-- it while fusion is happening. Hence this ugliness.
modifyWithStream :: G.Vector v a
                 => (forall s. G.Mutable v s a -> Stream b -> ST s ())
                 -> v a -> Stream b -> v a
{-# INLINE modifyWithStream #-}
modifyWithStream p v s = G.new (New.modifyWithStream p (G.clone v) s)
