{-# LANGUAGE FlexibleContexts #-}

module Data.IntervalIntMap.IntervalIntMap
    ( IntervalValue(..)
    , NaiveIntervalInt
    , intervalContains
    , partition
    ) where
import qualified Foreign.Storable as FS
import           Foreign.Ptr (castPtr, plusPtr)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Control.Monad.ST (runST)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Word (Word32)
import           Data.STRef


data IntervalValue = IntervalValue
                        { ivStart :: !Word32
                        , ivPast :: !Word32
                        , ivValue :: !Word32
                        }

instance FS.Storable IntervalValue where
    sizeOf _ = 3 * 4 -- aka 12
    alignment x = FS.alignment (ivStart x)
    peek p = IntervalValue
                    <$> FS.peek (castPtr p)
                    <*> FS.peek (castPtr p `plusPtr` 4)
                    <*> FS.peek (castPtr p `plusPtr` 8)
    poke ptr (IntervalValue s p v) = do
        FS.poke (castPtr ptr) s
        FS.poke (castPtr ptr `plusPtr` 4) p
        FS.poke (castPtr ptr `plusPtr` 8) v

intervalContains :: Int -> IntervalValue -> Bool
intervalContains p (IntervalValue s e _) =
    let p' = toEnum p
    in s <= p' && p' < e

type NaiveIntervalInt = VS.Vector IntervalValue

data IntervalIntMapNode = Leaf NaiveIntervalInt
                        | InnerNode
                            { nodeSplitValue :: !Int
                            , leftSplit :: !IntervalIntMapNode
                            , centerSplit :: !IntervalIntMapNode
                            , rightSplit :: !IntervalIntMapNode
                            }

data GrowableVector s = GrowableVector !Int !Int !(VSM.MVector s IntervalValue)

newGV :: (PrimMonad m) => m (GrowableVector (PrimState m))
newGV = GrowableVector 0 16 <$> VSM.unsafeNew 16

finalize ref = do
    GrowableVector used _ vec <- readSTRef ref
    VS.take used <$> VS.unsafeFreeze vec

pushBack val (GrowableVector used cap vec)
    | used == cap = do
        vec' <- VSM.grow vec (cap `div` 2)
        pushBack val (GrowableVector used (cap + (cap `div` 2)) vec')
    | otherwise = do
        VSM.write vec used val
        return $! GrowableVector (used+1) cap vec

partition :: Int -> NaiveIntervalInt -> (NaiveIntervalInt, NaiveIntervalInt, NaiveIntervalInt)
partition p vec = runST $ do
    left <- newSTRef =<< newGV
    center <- newSTRef =<< newGV
    right <- newSTRef =<< newGV
    VS.forM_ vec $ \val ->
        let target
                | ivPast val <= toEnum p = left
                | ivStart val > toEnum p = right
                | otherwise = center
        in 
            readSTRef target >>=
                pushBack val >>=
                writeSTRef target
    (,,)
        <$> finalize left
        <*> finalize center
        <*> finalize right

