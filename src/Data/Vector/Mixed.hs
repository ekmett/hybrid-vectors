{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
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

{-
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
-}
  ) where

import qualified Data.Vector.Generic as G
import Data.Vector.Mixed.Internal
-- import qualified Data.Vector.Fusion.Stream as Stream

-- import Control.DeepSeq ( NFData, rnf )
-- import Control.Monad ( MonadPlus(..), liftM, ap )
import Control.Monad.ST ( ST )
-- import Control.Monad.Primitive

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

import qualified Prelude

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
{-# INLINE headM #-}
headM = G.headM

-- | /O(1)/ Last element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
lastM :: (Monad m, G.Vector v a) => v a -> m a
{-# INLINE lastM #-}
lastM = G.lastM

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
-- explanation of why this is useful.
unsafeIndexM :: (Monad m, G.Vector v a) => v a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM = G.unsafeIndexM

-- | /O(1)/ First element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeHeadM :: (Monad m, G.Vector v a) => v a -> m a
{-# INLINE unsafeHeadM #-}
unsafeHeadM = G.unsafeHeadM

-- | /O(1)/ Last element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeLastM :: (Monad m, G.Vector v a) => v a -> m a
{-# INLINE unsafeLastM #-}
unsafeLastM = G.unsafeLastM


-- Extracting subvectors (slicing)
-- -------------------------------

-- | /O(1)/ Yield a slice of the vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Mixed u v a => Int   -- ^ @i@ starting index
                 -> Int   -- ^ @n@ length
                 -> v a
                 -> Vector a
{-# INLINE slice #-}
slice i j m = G.slice i j (mix m)

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty.
init :: Mixed u v a => v a -> Vector a
{-# INLINE init #-}
init m = G.init (mix m)

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty.
tail :: Mixed u v a => v a -> Vector a
{-# INLINE tail #-}
tail m = G.tail (mix m)

-- | /O(1)/ Yield at the first @n@ elements without copying. The vector may
-- contain less than @n@ elements in which case it is returned unchanged.
take :: Mixed u v a => Int -> v a -> Vector a
{-# INLINE take #-}
take i m = G.take i (mix m)

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector may
-- contain less than @n@ elements in which case an empty vector is returned.
drop :: Mixed u v a => Int -> v a -> Vector a
{-# INLINE drop #-}
drop i m = G.drop i (mix m)

-- | /O(1)/ Yield the first @n@ elements paired with the remainder without copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@
-- but slightly more efficient.
{-# INLINE splitAt #-}
splitAt :: Mixed u v a => Int -> v a -> (Vector a, Vector a)
splitAt i m = G.splitAt i (mix m)

-- | /O(1)/ Yield a slice of the vector without copying. The vector must
-- contain at least @i+n@ elements but this is not checked.
unsafeSlice :: Mixed u v a => Int   -- ^ @i@ starting index
                       -> Int   -- ^ @n@ length
                       -> v a
                       -> Vector a
{-# INLINE unsafeSlice #-}
unsafeSlice i j m = G.unsafeSlice i j (mix m)

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty but this is not checked.
unsafeInit :: Mixed u v a => v a -> Vector a
{-# INLINE unsafeInit #-}
unsafeInit m = G.unsafeInit (mix m)

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty but this is not checked.
unsafeTail :: Mixed u v a => v a -> Vector a
{-# INLINE unsafeTail #-}
unsafeTail m = G.unsafeTail (mix m)

-- | /O(1)/ Yield the first @n@ elements without copying. The vector must
-- contain at least @n@ elements but this is not checked.
unsafeTake :: Mixed u v a => Int -> v a -> Vector a
{-# INLINE unsafeTake #-}
unsafeTake i m = G.unsafeTake i (mix m)

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector
-- must contain at least @n@ elements but this is not checked.
unsafeDrop :: Mixed u v a => Int -> v a -> Vector a
{-# INLINE unsafeDrop #-}
unsafeDrop i m = G.unsafeDrop i (mix m)

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
cons a as = G.cons a (mix as)
{-# INLINE cons #-}

-- | /O(n)/ Append an element
snoc :: Mixed u v a => v a -> a -> Vector a
snoc as = G.snoc (mix as)
{-# INLINE snoc #-}

infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors
(++) :: (Mixed u v a, Mixed u' v' a) => v a -> v' a -> Vector a
m ++ n = mix m G.++ mix n
{-# INLINE (++) #-}

-- | /O(n)/ Concatenate all vectors in the list
concat :: Mixed u v a => [v a] -> Vector a
concat xs = G.concat (fmap mix xs)
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
force m = G.force (mix m)
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
m // xs = mix m G.// xs
{-# INLINE (//) #-}

-- | /O(m+n)/ For each pair @(i,a)@ from the vector of index/value pairs,
-- replace the vector element at position @i@ by @a@.
--
-- > update <5,9,2,7> <(2,1),(0,3),(2,8)> = <3,9,8,7>
--
update :: (Mixed u v a, Mixed u' v' (Int, a)) => v a        -- ^ initial vector (of length @m@)
       -> v' (Int, a) -- ^ vector of index/value pairs (of length @n@)
       -> Vector a
update m n = G.update (mix m) (mix n)
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
  ( Mixed u v a, Mixed u' v' Int, Mixed u'' v'' a
  ) => v a   -- ^ initial vector (of length @m@)
    -> v' Int -- ^ index vector (of length @n1@)
    -> v'' a   -- ^ value vector (of length @n2@)
    -> Vector a
update_ a b c = G.update_ (mix a) (mix b) (mix c)
{-# INLINE update_ #-}

-- | Same as ('//') but without bounds checking.
unsafeUpd :: Mixed u v a => v a -> [(Int, a)] -> Vector a
unsafeUpd m = G.unsafeUpd (mix m)
{-# INLINE unsafeUpd #-}

-- | Same as 'update' but without bounds checking.
unsafeUpdate :: (Mixed u v a, Mixed u' v' (Int, a)) => v a -> v' (Int, a) -> Vector a
unsafeUpdate m n = G.unsafeUpdate (mix m) (mix n)
{-# INLINE unsafeUpdate #-}

-- | Same as 'update_' but without bounds checking.
unsafeUpdate_ :: ( Mixed u v a, Mixed u' v' Int, Mixed u'' v'' a
  ) => v a -> v' Int -> v'' a -> Vector a
unsafeUpdate_ a b c = G.unsafeUpdate_ (mix a) (mix b) (mix c)
{-# INLINE unsafeUpdate_ #-}

{-
-- Accumulations
-- -------------

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the vector element
-- @a@ at position @i@ by @f a b@.
--
-- > accum (+) <5,9,2> [(2,4),(1,6),(0,3),(1,7)] = <5+3, 9+6+7, 2+4>
accum :: (a -> b -> a) -- ^ accumulating function @f@
      -> Vector a      -- ^ initial vector (of length @m@)
      -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
      -> Vector a
{-# INLINE accum #-}
accum = G.accum

-- | /O(m+n)/ For each pair @(i,b)@ from the vector of pairs, replace the vector
-- element @a@ at position @i@ by @f a b@.
--
-- > accumulate (+) <5,9,2> <(2,4),(1,6),(0,3),(1,7)> = <5+3, 9+6+7, 2+4>
accumulate :: (a -> b -> a)  -- ^ accumulating function @f@
            -> Vector a       -- ^ initial vector (of length @m@)
            -> Vector (Int,b) -- ^ vector of index/value pairs (of length @n@)
            -> Vector a
{-# INLINE accumulate #-}
accumulate = G.accumulate

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
accumulate_ :: (a -> b -> a) -- ^ accumulating function @f@
            -> Vector a      -- ^ initial vector (of length @m@)
            -> Vector Int    -- ^ index vector (of length @n1@)
            -> Vector b      -- ^ value vector (of length @n2@)
            -> Vector a
{-# INLINE accumulate_ #-}
accumulate_ = G.accumulate_

-- | Same as 'accum' but without bounds checking.
unsafeAccum :: (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE unsafeAccum #-}
unsafeAccum = G.unsafeAccum

-- | Same as 'accumulate' but without bounds checking.
unsafeAccumulate :: (a -> b -> a) -> Vector a -> Vector (Int,b) -> Vector a
{-# INLINE unsafeAccumulate #-}
unsafeAccumulate = G.unsafeAccumulate

-- | Same as 'accumulate_' but without bounds checking.
unsafeAccumulate_
  :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ = G.unsafeAccumulate_

-- Permutations
-- ------------

-- | /O(n)/ Reverse a vector
reverse :: Vector a -> Vector a
{-# INLINE reverse #-}
reverse = G.reverse

-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @xs'!'i@. This is equivalent to @'map' (xs'!') is@ but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute :: Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute = G.backpermute

-- | Same as 'backpermute' but without bounds checking.
unsafeBackpermute :: Vector a -> Vector Int -> Vector a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute = G.unsafeBackpermute

-- Safe destructive updates
-- ------------------------

-- | Apply a destructive operation to a vector. The operation will be
-- performed in place if it is safe to do so and will modify a copy of the
-- vector otherwise.
--
-- @
-- modify (\\v -> write v 0 \'x\') ('replicate' 3 \'a\') = \<\'x\',\'a\',\'a\'\>
-- @
modify :: (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
{-# INLINE modify #-}
modify p = G.modify p

-- Indexing
-- --------

-- | /O(n)/ Pair each element in a vector with its index
indexed :: Vector a -> Vector (Int,a)
{-# INLINE indexed #-}
indexed = G.indexed

-- Mapping
-- -------

-- | /O(n)/ Map a function over a vector
map :: (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map = G.map

-- | /O(n)/ Apply a function to every element of a vector and its index
imap :: (Int -> a -> b) -> Vector a -> Vector b
{-# INLINE imap #-}
imap = G.imap

-- | Map a function over a vector and concatenate the results.
concatMap :: (a -> Vector b) -> Vector a -> Vector b
{-# INLINE concatMap #-}
concatMap = G.concatMap

-- Monadic mapping
-- ---------------

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results
mapM :: Monad m => (a -> m b) -> Vector a -> m (Vector b)
{-# INLINE mapM #-}
mapM = G.mapM

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results
mapM_ :: Monad m => (a -> m b) -> Vector a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results. Equvalent to @flip 'mapM'@.
forM :: Monad m => Vector a -> (a -> m b) -> m (Vector b)
{-# INLINE forM #-}
forM = G.forM

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: Monad m => Vector a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- Zipping
-- -------

-- | /O(min(m,n))/ Zip two vectors with the given function.
zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith = G.zipWith

-- | Zip three vectors with the given function.
zipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE zipWith3 #-}
zipWith3 = G.zipWith3

zipWith4 :: (a -> b -> c -> d -> e)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE zipWith4 #-}
zipWith4 = G.zipWith4

zipWith5 :: (a -> b -> c -> d -> e -> f)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f
{-# INLINE zipWith5 #-}
zipWith5 = G.zipWith5

zipWith6 :: (a -> b -> c -> d -> e -> f -> g)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f -> Vector g
{-# INLINE zipWith6 #-}
zipWith6 = G.zipWith6

-- | /O(min(m,n))/ Zip two vectors with a function that also takes the
-- elements' indices.
izipWith :: (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE izipWith #-}
izipWith = G.izipWith

-- | Zip three vectors and their indices with the given function.
izipWith3 :: (Int -> a -> b -> c -> d)
          -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE izipWith3 #-}
izipWith3 = G.izipWith3

izipWith4 :: (Int -> a -> b -> c -> d -> e)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE izipWith4 #-}
izipWith4 = G.izipWith4

izipWith5 :: (Int -> a -> b -> c -> d -> e -> f)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f
{-# INLINE izipWith5 #-}
izipWith5 = G.izipWith5

izipWith6 :: (Int -> a -> b -> c -> d -> e -> f -> g)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f -> Vector g
{-# INLINE izipWith6 #-}
izipWith6 = G.izipWith6

-- | Elementwise pairing of array elements.
zip :: Vector a -> Vector b -> Vector (a, b)
{-# INLINE zip #-}
zip = G.zip

-- | zip together three vectors into a vector of triples
zip3 :: Vector a -> Vector b -> Vector c -> Vector (a, b, c)
{-# INLINE zip3 #-}
zip3 = G.zip3

zip4 :: Vector a -> Vector b -> Vector c -> Vector d
     -> Vector (a, b, c, d)
{-# INLINE zip4 #-}
zip4 = G.zip4

zip5 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e
     -> Vector (a, b, c, d, e)
{-# INLINE zip5 #-}
zip5 = G.zip5

zip6 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
     -> Vector (a, b, c, d, e, f)
{-# INLINE zip6 #-}
zip6 = G.zip6

-- Unzipping
-- ---------

-- | /O(min(m,n))/ Unzip a vector of pairs.
unzip :: Vector (a, b) -> (Vector a, Vector b)
{-# INLINE unzip #-}
unzip = G.unzip

unzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)
{-# INLINE unzip3 #-}
unzip3 = G.unzip3

unzip4 :: Vector (a, b, c, d) -> (Vector a, Vector b, Vector c, Vector d)
{-# INLINE unzip4 #-}
unzip4 = G.unzip4

unzip5 :: Vector (a, b, c, d, e)
       -> (Vector a, Vector b, Vector c, Vector d, Vector e)
{-# INLINE unzip5 #-}
unzip5 = G.unzip5

unzip6 :: Vector (a, b, c, d, e, f)
       -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f)
{-# INLINE unzip6 #-}
unzip6 = G.unzip6

-- Monadic zipping
-- ---------------

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and yield a
-- vector of results
zipWithM :: Monad m => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
{-# INLINE zipWithM #-}
zipWithM = G.zipWithM

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and ignore the
-- results
zipWithM_ :: Monad m => (a -> b -> m c) -> Vector a -> Vector b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ = G.zipWithM_

-- Filtering
-- ---------

-- | /O(n)/ Drop elements that do not satisfy the predicate
filter :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter = G.filter

-- | /O(n)/ Drop elements that do not satisfy the predicate which is applied to
-- values and their indices
ifilter :: (Int -> a -> Bool) -> Vector a -> Vector a
{-# INLINE ifilter #-}
ifilter = G.ifilter

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate
filterM :: Monad m => (a -> m Bool) -> Vector a -> m (Vector a)
{-# INLINE filterM #-}
filterM = G.filterM

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate
-- without copying.
takeWhile :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile = G.takeWhile

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- without copying.
dropWhile :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile = G.dropWhile

-- Parititioning
-- -------------

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a sometimes
-- reduced performance compared to 'unstablePartition'.
partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE partition #-}
partition = G.partition

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't.
-- The order of the elements is not preserved but the operation is often
-- faster than 'partition'.
unstablePartition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE unstablePartition #-}
unstablePartition = G.unstablePartition

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest without copying.
span :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE span #-}
span = G.span

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
break :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE break #-}
break = G.break

-- Searching
-- ---------

infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element
elem :: Eq a => a -> Vector a -> Bool
{-# INLINE elem #-}
elem = G.elem

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem')
notElem :: Eq a => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem = G.notElem

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find = G.find

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = G.findIndex

-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
findIndices :: (a -> Bool) -> Vector a -> Vector Int
{-# INLINE findIndices #-}
findIndices = G.findIndices

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: Eq a => a -> Vector a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex = G.elemIndex

-- | /O(n)/ Yield the indices of all occurences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
elemIndices :: Eq a => a -> Vector a -> Vector Int
{-# INLINE elemIndices #-}
elemIndices = G.elemIndices

-- Folding
-- -------

-- | /O(n)/ Left fold
foldl :: (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl = G.foldl

-- | /O(n)/ Left fold on non-empty vectors
foldl1 :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 = G.foldl1

-- | /O(n)/ Left fold with strict accumulator
foldl' :: (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator
foldl1' :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' = G.foldl1'

-- | /O(n)/ Right fold
foldr :: (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr = G.foldr

-- | /O(n)/ Right fold on non-empty vectors
foldr1 :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1 #-}
foldr1 = G.foldr1

-- | /O(n)/ Right fold with a strict accumulator
foldr' :: (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator
foldr1' :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1' #-}
foldr1' = G.foldr1'

-- | /O(n)/ Left fold (function applied to each element and its index)
ifoldl :: (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | /O(n)/ Left fold with strict accumulator (function applied to each element
-- and its index)
ifoldl' :: (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | /O(n)/ Right fold (function applied to each element and its index)
ifoldr :: (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | /O(n)/ Right fold with strict accumulator (function applied to each
-- element and its index)
ifoldr' :: (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- Specialised folds
-- -----------------

-- | /O(n)/ Check if all elements satisfy the predicate.
all :: (a -> Bool) -> Vector a -> Bool
{-# INLINE all #-}
all = G.all

-- | /O(n)/ Check if any element satisfies the predicate.
any :: (a -> Bool) -> Vector a -> Bool
{-# INLINE any #-}
any = G.any

-- | /O(n)/ Check if all elements are 'True'
and :: Vector Bool -> Bool
{-# INLINE and #-}
and = G.and

-- | /O(n)/ Check if any element is 'True'
or :: Vector Bool -> Bool
{-# INLINE or #-}
or = G.or

-- | /O(n)/ Compute the sum of the elements
sum :: Num a => Vector a -> a
{-# INLINE sum #-}
sum = G.sum

-- | /O(n)/ Compute the produce of the elements
product :: Num a => Vector a -> a
{-# INLINE product #-}
product = G.product

-- | /O(n)/ Yield the maximum element of the vector. The vector may not be
-- empty.
maximum :: Ord a => Vector a -> a
{-# INLINE maximum #-}
maximum = G.maximum

-- | /O(n)/ Yield the maximum element of the vector according to the given
-- comparison function. The vector may not be empty.
maximumBy :: (a -> a -> Ordering) -> Vector a -> a
{-# INLINE maximumBy #-}
maximumBy = G.maximumBy

-- | /O(n)/ Yield the minimum element of the vector. The vector may not be
-- empty.
minimum :: Ord a => Vector a -> a
{-# INLINE minimum #-}
minimum = G.minimum

-- | /O(n)/ Yield the minimum element of the vector according to the given
-- comparison function. The vector may not be empty.
minimumBy :: (a -> a -> Ordering) -> Vector a -> a
{-# INLINE minimumBy #-}
minimumBy = G.minimumBy

-- | /O(n)/ Yield the index of the maximum element of the vector. The vector
-- may not be empty.
maxIndex :: Ord a => Vector a -> Int
{-# INLINE maxIndex #-}
maxIndex = G.maxIndex

-- | /O(n)/ Yield the index of the maximum element of the vector according to
-- the given comparison function. The vector may not be empty.
maxIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy = G.maxIndexBy

-- | /O(n)/ Yield the index of the minimum element of the vector. The vector
-- may not be empty.
minIndex :: Ord a => Vector a -> Int
{-# INLINE minIndex #-}
minIndex = G.minIndex

-- | /O(n)/ Yield the index of the minimum element of the vector according to
-- the given comparison function. The vector may not be empty.
minIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE minIndexBy #-}
minIndexBy = G.minIndexBy

-- Monadic folds
-- -------------

-- | /O(n)/ Monadic fold
foldM :: Monad m => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM #-}
foldM = G.foldM

-- | /O(n)/ Monadic fold over non-empty vectors
fold1M :: Monad m => (a -> a -> m a) -> Vector a -> m a
{-# INLINE fold1M #-}
fold1M = G.fold1M

-- | /O(n)/ Monadic fold with strict accumulator
foldM' :: Monad m => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
fold1M' :: Monad m => (a -> a -> m a) -> Vector a -> m a
{-# INLINE fold1M' #-}
fold1M' = G.fold1M'

-- | /O(n)/ Monadic fold that discards the result
foldM_ :: Monad m => (a -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE foldM_ #-}
foldM_ = G.foldM_

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result
fold1M_ :: Monad m => (a -> a -> m a) -> Vector a -> m ()
{-# INLINE fold1M_ #-}
fold1M_ = G.fold1M_

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
foldM'_ :: Monad m => (a -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE foldM'_ #-}
foldM'_ = G.foldM'_

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
-- that discards the result
fold1M'_ :: Monad m => (a -> a -> m a) -> Vector a -> m ()
{-# INLINE fold1M'_ #-}
fold1M'_ = G.fold1M'_

-- Monadic sequencing
-- ------------------

-- | Evaluate each action and collect the results
sequence :: Monad m => Vector (m a) -> m (Vector a)
{-# INLINE sequence #-}
sequence = G.sequence

-- | Evaluate each action and discard the results
sequence_ :: Monad m => Vector (m a) -> m ()
{-# INLINE sequence_ #-}
sequence_ = G.sequence_

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
prescanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl #-}
prescanl = G.prescanl

-- | /O(n)/ Prescan with strict accumulator
prescanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl' #-}
prescanl' = G.prescanl'

-- | /O(n)/ Scan
--
-- @
-- postscanl f z = 'tail' . 'scanl' f z
-- @
--
-- Example: @postscanl (+) 0 \<1,2,3,4\> = \<1,3,6,10\>@
--
postscanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl #-}
postscanl = G.postscanl

-- | /O(n)/ Scan with strict accumulator
postscanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl' #-}
postscanl' = G.postscanl'

-- | /O(n)/ Haskell-style scan
--
-- > scanl f z <x1,...,xn> = <y1,...,y(n+1)>
-- >   where y1 = z
-- >         yi = f y(i-1) x(i-1)
--
-- Example: @scanl (+) 0 \<1,2,3,4\> = \<0,1,3,6,10\>@
--
scanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl #-}
scanl = G.scanl

-- | /O(n)/ Haskell-style scan with strict accumulator
scanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl' #-}
scanl' = G.scanl'

-- | /O(n)/ Scan over a non-empty vector
--
-- > scanl f <x1,...,xn> = <y1,...,yn>
-- >   where y1 = x1
-- >         yi = f y(i-1) xi
--
scanl1 :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1 #-}
scanl1 = G.scanl1

-- | /O(n)/ Scan over a non-empty vector with a strict accumulator
scanl1' :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1' #-}
scanl1' = G.scanl1'

-- | /O(n)/ Right-to-left prescan
--
-- @
-- prescanr f z = 'reverse' . 'prescanl' (flip f) z . 'reverse'
-- @
--
prescanr :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr #-}
prescanr = G.prescanr

-- | /O(n)/ Right-to-left prescan with strict accumulator
prescanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr' #-}
prescanr' = G.prescanr'

-- | /O(n)/ Right-to-left scan
postscanr :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr #-}
postscanr = G.postscanr

-- | /O(n)/ Right-to-left scan with strict accumulator
postscanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr' #-}
postscanr' = G.postscanr'

-- | /O(n)/ Right-to-left Haskell-style scan
scanr :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr #-}
scanr = G.scanr

-- | /O(n)/ Right-to-left Haskell-style scan with strict accumulator
scanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr' #-}
scanr' = G.scanr'

-- | /O(n)/ Right-to-left scan over a non-empty vector
scanr1 :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1 #-}
scanr1 = G.scanr1

-- | /O(n)/ Right-to-left scan over a non-empty vector with a strict
-- accumulator
scanr1' :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1' #-}
scanr1' = G.scanr1'

-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert a vector to a list
toList :: Vector a -> [a]
{-# INLINE toList #-}
toList = G.toList

-- | /O(n)/ Convert a list to a vector
fromList :: [a] -> Vector a
{-# INLINE fromList #-}
fromList = G.fromList

-- | /O(n)/ Convert the first @n@ elements of a list to a vector
--
-- @
-- fromListN n xs = 'fromList' ('take' n xs)
-- @
fromListN :: Int -> [a] -> Vector a
{-# INLINE fromListN #-}
fromListN = G.fromListN

-- Conversions - Mutable vectors
-- -----------------------------

-- | /O(1)/ Unsafe convert a mutable vector to an immutable one without
-- copying. The mutable vector may not be used after this operation.
unsafeFreeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze = G.unsafeFreeze

-- | /O(1)/ Unsafely convert an immutable vector to a mutable one without
-- copying. The immutable vector may not be used after this operation.
unsafeThaw :: PrimMonad m => Vector a -> m (MVector (PrimState m) a)
{-# INLINE unsafeThaw #-}
unsafeThaw = G.unsafeThaw

-- | /O(n)/ Yield a mutable copy of the immutable vector.
thaw :: PrimMonad m => Vector a -> m (MVector (PrimState m) a)
{-# INLINE thaw #-}
thaw = G.thaw

-- | /O(n)/ Yield an immutable copy of the mutable vector.
freeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)
{-# INLINE freeze #-}
freeze = G.freeze

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length. This is not checked.
unsafeCopy :: PrimMonad m => MVector (PrimState m) a -> Vector a -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length.
copy :: PrimMonad m => MVector (PrimState m) a -> Vector a -> m ()
{-# INLINE copy #-}
copy = G.copy
-}
