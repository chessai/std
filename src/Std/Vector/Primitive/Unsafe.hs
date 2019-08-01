-- | Unoxed @Vector@ unsafe functions. These perform no bounds
--   checking, and may cause segmentation faults etc.!  Import as:
--
-- > import qualified Std.Vector.Unoxed.Unsafe as VU'
module Std.Vector.Primitive.Unsafe
  (
  -- * Accessors
  -- ** Indexing
    Data.Vector.Primitive.unsafeIndex
  , Data.Vector.Primitive.unsafeHead
  , Data.Vector.Primitive.unsafeLast

  -- ** Monadic indexing
  , Data.Vector.Primitive.unsafeIndexM
  , Data.Vector.Primitive.unsafeHeadM
  , Data.Vector.Primitive.unsafeLastM

  -- ** Extracting subvectors
  , Data.Vector.Primitive.unsafeSlice
  , Data.Vector.Primitive.unsafeInit
  , Data.Vector.Primitive.unsafeTail
  , Data.Vector.Primitive.unsafeTake
  , Data.Vector.Primitive.unsafeDrop

  -- * Modifying vectors
  -- ** Bulk updates
  , Data.Vector.Primitive.unsafeUpd
  , Data.Vector.Primitive.unsafeUpdate_

  -- ** Accumulations
  , Data.Vector.Primitive.unsafeAccum
  , Data.Vector.Primitive.unsafeAccumulate_

  -- ** Permutations
  , Data.Vector.Primitive.unsafeBackpermute

  -- * Conversions
  -- ** Mutable vectors
  , Data.Vector.Primitive.unsafeFreeze
  , Data.Vector.Primitive.unsafeThaw
  , Data.Vector.Primitive.unsafeCopy
  ) where

import qualified Data.Vector.Primitive
