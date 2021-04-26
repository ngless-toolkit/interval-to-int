{-# LANGUAGE FlexibleContexts #-}

module Data.IntervalIntMap.Internal.GrowableVector
    ( GrowableVector
    , GrowableVectorData(..)
    , new
    , pushBack
    , unsafeFreeze
    , length
    ) where

import Prelude hiding (length)

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Foreign.Storable (Storable(..))
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)

{--| This is a growable vector (i.e., one that includes the 'pushBack'
 - function) which must exist in a 'PrimMonad'. It only supports Storable data
 - items.
 -}


type GrowableVector s a = MutVar s (GrowableVectorData s a)

data GrowableVectorData s a = GrowableVectorData !Int !(VSM.MVector s a)

-- | Empty vector
new :: (PrimMonad m, Storable a) => m (GrowableVector (PrimState m) a)
new = do
    vd <- GrowableVectorData 0 <$> VSM.unsafeNew 16
    newMutVar vd

-- | Insert an element at the end of the vector
pushBack :: (PrimMonad m, Storable a) => a -> GrowableVector (PrimState m) a -> m ()
pushBack val gv =
    readMutVar gv >>= pushBack' val >>= writeMutVar gv

pushBack' :: (PrimMonad m, Storable a) => a -> GrowableVectorData (PrimState m) a -> m (GrowableVectorData (PrimState m) a)
pushBack' val (GrowableVectorData used vec)
    | used == VSM.length vec = do
        vec' <- VSM.grow vec (VSM.length vec `div` 2) -- multiplying by 1.5 is close to optimal
        pushBack' val (GrowableVectorData  used vec')
    | otherwise = do
        VSM.write vec used val
        return $! GrowableVectorData (used+1) vec

-- | This operation is unsafe as original vector should not be used again!
unsafeFreeze :: (PrimMonad m, Storable a) => GrowableVector (PrimState m) a -> m (VS.Vector a)
unsafeFreeze gv = do
    GrowableVectorData used vec <- readMutVar gv
    VS.take used <$> VS.unsafeFreeze vec

-- | Return the current number of stored elements
length :: (PrimMonad m, Storable a) => GrowableVector (PrimState m) a -> m Int
length gv = do
    GrowableVectorData len _ <- readMutVar gv
    return len
