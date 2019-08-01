{-# LANGUAGE CPP #-}

-- | Primitive @Vector@. Import as:
--
-- > import qualified Std.Vector.Primitive as VU
--
-- This module does not export any partial or unsafe functions.  For those, see
-- "Std.Vector.Primitive.Partial" and "Std.Vector.Primitive.Unsafe"
module Std.Vector.Primitive
  (
  -- * Primitive vectors
    Data.Vector.Primitive.Vector
  , Data.Vector.Primitive.MVector(..)
  , Data.Vector.Primitive.Prim

  -- * Accessors
  -- ** Length information
  , Data.Vector.Primitive.length
  , Data.Vector.Primitive.null

  -- ** Indexing
  , (Data.Vector.Primitive.!?)

  -- ** Extracting subvectors
  , Data.Vector.Primitive.slice
  , Data.Vector.Primitive.take
  , Data.Vector.Primitive.drop
  , Data.Vector.Primitive.splitAt

  -- * Construction
  -- ** Initialisation
  , Data.Vector.Primitive.empty
  , Data.Vector.Primitive.singleton
  , Data.Vector.Primitive.replicate
  , Data.Vector.Primitive.generate
  , Data.Vector.Primitive.iterateN

  -- ** Monadic initialisation
  , Data.Vector.Primitive.replicateM
  , Data.Vector.Primitive.generateM
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Primitive.iterateNM
#endif
  , Data.Vector.Primitive.create
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Primitive.createT
#endif

  -- ** Unfolding
  , Data.Vector.Primitive.unfoldr
  , Data.Vector.Primitive.unfoldrN
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Primitive.unfoldrM
  , Data.Vector.Primitive.unfoldrNM
#endif
  , Data.Vector.Primitive.constructN
  , Data.Vector.Primitive.constructrN

  -- ** Enumeration
  , Data.Vector.Primitive.enumFromN
  , Data.Vector.Primitive.enumFromStepN
  , Data.Vector.Primitive.enumFromTo
  , Data.Vector.Primitive.enumFromThenTo

  -- ** Concatenation
  , Data.Vector.Primitive.cons
  , Data.Vector.Primitive.snoc
  , (Data.Vector.Primitive.++)
  , Data.Vector.Primitive.concat

  -- ** Restricting memory usage
  , Data.Vector.Primitive.force

  -- * Modifying vectors
  -- ** Permutations
  , Data.Vector.Primitive.reverse

  -- ** Safe destructive update
  , Data.Vector.Primitive.modify

  -- * Elementwise operations
  -- ** Mapping
  , Data.Vector.Primitive.map
  , Data.Vector.Primitive.imap
  , Data.Vector.Primitive.concatMap

  -- ** Monadic mapping
  , Data.Vector.Primitive.mapM
  , imapM
  , Data.Vector.Primitive.mapM_
  , imapM_
  , Data.Vector.Primitive.forM
  , Data.Vector.Primitive.forM_

  -- ** Zipping
  , Data.Vector.Primitive.zipWith
  , Data.Vector.Primitive.zipWith3
  , Data.Vector.Primitive.zipWith4
  , Data.Vector.Primitive.zipWith5
  , Data.Vector.Primitive.zipWith6
  , Data.Vector.Primitive.izipWith
  , Data.Vector.Primitive.izipWith3
  , Data.Vector.Primitive.izipWith4
  , Data.Vector.Primitive.izipWith5
  , Data.Vector.Primitive.izipWith6

  -- ** Monadic zipping
  , zipWithM
  , izipWithM
  , zipWithM_
  , izipWithM_

  -- * Working with predicates
  -- ** Filtering
  , Data.Vector.Primitive.filter
  , Data.Vector.Primitive.ifilter
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Primitive.uniq
  , Data.Vector.Primitive.mapMaybe
  , Data.Vector.Primitive.imapMaybe
#endif
  , Data.Vector.Primitive.filterM
  , Data.Vector.Primitive.takeWhile
  , Data.Vector.Primitive.dropWhile

  -- ** Partitioning
  , Data.Vector.Primitive.partition
  , Data.Vector.Primitive.unstablePartition
  , Data.Vector.Primitive.span
  , Data.Vector.Primitive.break

  -- ** Searching
  , Data.Vector.Primitive.elem
  , Data.Vector.Primitive.notElem
  , Data.Vector.Primitive.find
  , Data.Vector.Primitive.findIndex
  , Data.Vector.Primitive.findIndices
  , Data.Vector.Primitive.elemIndex
  , Data.Vector.Primitive.elemIndices

  -- * Folding
  , Data.Vector.Primitive.foldl
  , Data.Vector.Primitive.foldl'
  , Data.Vector.Primitive.foldr
  , Data.Vector.Primitive.foldr'
  , Data.Vector.Primitive.ifoldl
  , Data.Vector.Primitive.ifoldl'
  , Data.Vector.Primitive.ifoldr
  , Data.Vector.Primitive.ifoldr'

  -- ** Specialised folds
  , Data.Vector.Primitive.all
  , Data.Vector.Primitive.any
  , Data.Vector.Primitive.sum
  , Data.Vector.Primitive.product

  -- ** Monadic folds
  , Data.Vector.Primitive.foldM
  , ifoldM
  , Data.Vector.Primitive.foldM'
  , ifoldM'
  , Data.Vector.Primitive.foldM_
  , ifoldM_
  , Data.Vector.Primitive.foldM'_
  , ifoldM'_

  -- * Prefix sums (scans)
  , Data.Vector.Primitive.prescanl
  , Data.Vector.Primitive.prescanl'
  , Data.Vector.Primitive.postscanl
  , Data.Vector.Primitive.postscanl'
  , Data.Vector.Primitive.scanl
  , Data.Vector.Primitive.scanl'
  , Data.Vector.Primitive.prescanr
  , Data.Vector.Primitive.prescanr'
  , Data.Vector.Primitive.postscanr
  , Data.Vector.Primitive.postscanr'
  , Data.Vector.Primitive.scanr
  , Data.Vector.Primitive.scanr'

  -- * Conversions
  -- ** Lists
  , Data.Vector.Primitive.toList
  , Data.Vector.Primitive.fromList
  , Data.Vector.Primitive.fromListN

  -- ** Different vector types
  , Data.Vector.Primitive.convert

  -- ** Mutable vectors
  , Data.Vector.Primitive.freeze
  , Data.Vector.Primitive.thaw
  , Data.Vector.Primitive.copy
  ) where

import qualified Data.Vector.Primitive

import Data.Vector.Primitive (Vector, Prim)
import qualified Data.Vector.Generic as G

imapM :: (Monad m, Prim a, Prim b)
  => (Int -> a -> m b)
  -> Vector a
  -> m (Vector b)
{-# INLINE imapM #-}
imapM = G.imapM

imapM_ :: (Monad m, Prim a)
  => (Int -> a -> m b)
  -> Vector a
  -> m ()
{-# INLINE imapM_ #-}
imapM_ = G.imapM_

zipWithM :: (Monad m, Prim a, Prim b, Prim c)
  => (a -> b -> m c)
  -> Vector a
  -> Vector b
  -> m (Vector c)
{-# INLINE zipWithM #-}
zipWithM = G.zipWithM

zipWithM_ :: (Monad m, Prim a, Prim b)
  => (a -> b -> m c)
  -> Vector a
  -> Vector b
  -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ = G.zipWithM_

izipWithM :: (Monad m, Prim a, Prim b, Prim c)
  => (Int -> a -> b -> m c)
  -> Vector a
  -> Vector b
  -> m (Vector c)
{-# INLINE izipWithM #-}
izipWithM = G.izipWithM

izipWithM_ :: (Monad m, Prim a, Prim b)
  => (Int -> a -> b -> m c)
  -> Vector a
  -> Vector b
  -> m ()
{-# INLINE izipWithM_ #-}
izipWithM_ = G.izipWithM_

ifoldM :: (Monad m, Prim b)
  => (a -> Int -> b -> m a)
  -> a
  -> Vector b
  -> m a
{-# INLINE ifoldM #-}
ifoldM = G.ifoldM

ifoldM' :: (Monad m, Prim b)
  => (a -> Int -> b -> m a)
  -> a
  -> Vector b
  -> m a
{-# INLINE ifoldM' #-}
ifoldM' = G.ifoldM'

ifoldM_ :: (Monad m, Prim b)
  => (a -> Int -> b -> m a)
  -> a
  -> Vector b
  -> m ()
{-# INLINE ifoldM_ #-}
ifoldM_ = G.ifoldM_

ifoldM'_ :: (Monad m, Prim b)
  => (a -> Int -> b -> m a)
  -> a
  -> Vector b
  -> m ()
{-# INLINE ifoldM'_ #-}
ifoldM'_ = G.ifoldM'_

