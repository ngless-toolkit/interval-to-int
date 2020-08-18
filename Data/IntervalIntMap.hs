{-# LANGUAGE FlexibleContexts #-}

module Data.IntervalIntMap
#ifndef IS_BUILDING_TEST
    ( IntervalIntMap
#else
    ( IntervalIntMap(..)
#endif
    , IntervalIntMapAccumulator
    , IM.Interval(..)
    , new
    , insert
    , freeze
    , lookup
    , overlaps
    ) where

import Prelude hiding (lookup)

import qualified Data.IntervalIntMap.Internal.IntervalIntIntMap as IM
import qualified Data.IntervalIntMap.Internal.GrowableVector as GV
import qualified Data.Vector.Storable as VS
import qualified Data.IntSet as IS
import           Foreign.Storable (Storable(..))
import           Control.Monad.Primitive (PrimMonad, PrimState)


{-| The typical interval map structure models a function of the type @ f :: Int
 - -> Maybe a@. That is, each position in the domain is either annotated by an
 - interval or it is not.
 -
 - When you attempt to insert an interval that overlaps with an existing one,
 - the new value may either (1) replace or (2) by combined with the older one.
 -
 -
 - This is **not** the model here. The model here is @f :: Int -> [a]@! An
 - interval map is a bag of intervals which may overlap (or not). When they do
 - overlap and you query at a position where multiple ones could be active, you
 - get all of them (in some order).
 -
 -}


data IntervalIntMap a = IntervalIntMap !IM.IntervalIntMap
                                       !(VS.Vector a)


data IntervalIntMapAccumulator s a = IntervalIntMapAccumulator
                                        !(GV.GrowableVector s (IM.IntervalValue))
                                        !(GV.GrowableVector s a)

new :: (PrimMonad m, Storable a) => m (IntervalIntMapAccumulator (PrimState m) a)
new = IntervalIntMapAccumulator <$> GV.new <*> GV.new


insert :: (PrimMonad m, Storable a) => IM.Interval -> a -> IntervalIntMapAccumulator (PrimState m) a -> m ()
insert (IM.Interval s e) v (IntervalIntMapAccumulator ivs dat) = do
    ix <- GV.length dat
    GV.pushBack v dat
    GV.pushBack (IM.IntervalValue (toEnum s) (toEnum e) (toEnum ix)) ivs


freeze :: (PrimMonad m, Storable a) => IntervalIntMapAccumulator (PrimState m) a -> m (IntervalIntMap a)
freeze (IntervalIntMapAccumulator ivs values) =
    IntervalIntMap
        <$> (IM.freeze <$> GV.unsafeFreeze ivs)
        <*> GV.unsafeFreeze values

indexAll :: Storable a => VS.Vector a -> IS.IntSet -> [a]
indexAll values = (fmap $ (VS.!) values) . IS.toList

lookup ::  Storable a => Int -> IntervalIntMap a -> [a]
lookup p (IntervalIntMap imap values) = indexAll values $ IM.lookup p imap

overlaps :: Storable a => IM.Interval -> IntervalIntMap a -> [a]
overlaps i (IntervalIntMap imap values) = indexAll values $ IM.overlaps i imap