-- | Primitive @Vector@ partial functions. Import as:
--
-- > import qualified Std.Vector.Primitive.Partial as VU'
module Std.Vector.Primitive.Partial
  (
  -- * Accessors
  -- ** Indexing
    (Data.Vector.Primitive.!)
  , Data.Vector.Primitive.head
  , Data.Vector.Primitive.last

  -- ** Monadic indexing
  , Data.Vector.Primitive.indexM
  , Data.Vector.Primitive.headM
  , Data.Vector.Primitive.lastM

  -- ** Extracting subvectors
  , Data.Vector.Primitive.init
  , Data.Vector.Primitive.tail

  -- * Modifying vectors
  -- ** Bulk updates
  , (Data.Vector.Primitive.//)

  -- ** Accumulations
  , Data.Vector.Primitive.accum
  , Data.Vector.Primitive.accumulate_

  -- ** Permutations
  , Data.Vector.Primitive.backpermute

  -- * Folding
  , Data.Vector.Primitive.foldl1
  , Data.Vector.Primitive.foldl1'
  , Data.Vector.Primitive.foldr1
  , Data.Vector.Primitive.foldr1'

  -- ** Specialised folds
  , Data.Vector.Primitive.maximum
  , Data.Vector.Primitive.maximumBy
  , Data.Vector.Primitive.minimum
  , Data.Vector.Primitive.minimumBy
  , Data.Vector.Primitive.minIndex
  , Data.Vector.Primitive.minIndexBy
  , Data.Vector.Primitive.maxIndex
  , Data.Vector.Primitive.maxIndexBy

  -- ** Monadic folds
  , Data.Vector.Primitive.fold1M
  , Data.Vector.Primitive.fold1M'
  , Data.Vector.Primitive.fold1M_
  , Data.Vector.Primitive.fold1M'_

  -- * Prefix sums (scans)
  , Data.Vector.Primitive.scanl1
  , Data.Vector.Primitive.scanl1'
  , Data.Vector.Primitive.scanr1
  , Data.Vector.Primitive.scanr1'
  ) where

import qualified Data.Vector.Primitive
