{-# LANGUAGE FlexibleContexts #-}

module Data.IntervalIntMap.Internal.GrowableVector
    ( GrowableVector
    , GrowableVectorData(..)
    , new
    , pushBack
    , unsafeFreeze
    ) where

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Foreign.Storable (Storable(..))
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)


type GrowableVector s a = MutVar s (GrowableVectorData s a)

data GrowableVectorData s a = GrowableVectorData !Int !(VSM.MVector s a)

new :: (PrimMonad m, Storable a) => m (GrowableVector (PrimState m) a)
new = do
    vd <- GrowableVectorData 0 <$> VSM.unsafeNew 16
    newMutVar vd

pushBack :: (PrimMonad m, Storable a) => a -> GrowableVector (PrimState m) a -> m ()
pushBack val gv =
    readMutVar gv >>= pushBack' val >>= writeMutVar gv

pushBack' :: (PrimMonad m, Storable a) => a -> GrowableVectorData (PrimState m) a -> m (GrowableVectorData (PrimState m) a)
pushBack' val (GrowableVectorData used vec)
    | used == VSM.length vec = do
        vec' <- VSM.grow vec (VSM.length vec `div` 2)
        pushBack' val (GrowableVectorData  used vec')
    | otherwise = do
        VSM.write vec used val
        return $! GrowableVectorData (used+1) vec

unsafeFreeze :: (PrimMonad m, Storable a) => GrowableVector (PrimState m) a -> m (VS.Vector a)
unsafeFreeze gv = do
    GrowableVectorData used vec <- readMutVar gv
    VS.take used <$> VS.unsafeFreeze vec

