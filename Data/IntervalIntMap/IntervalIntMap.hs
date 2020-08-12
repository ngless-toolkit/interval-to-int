{-# LANGUAGE FlexibleContexts #-}

module Data.IntervalIntMap.IntervalIntMap
    ( IntervalValue(..)
    , IntervalIntMap
    , naiveIntervalMapFind
    , intervalMapFind
    , NaiveIntervalInt
    , intervalContains
    , partition
    , mkTree
    ) where
import qualified Foreign.Storable as FS
import           Foreign.Ptr (castPtr, plusPtr)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Control.Monad.ST (runST)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Word (Word32)
import           Data.Ord (comparing)
import           Data.STRef (readSTRef, newSTRef, writeSTRef)
import           Data.Vector.Algorithms.Tim (sortBy)


{- DATA STRUCTURE
 -
 - An IntervalValue contains the interval [ivStart, ivPast) and the value. This
 - is a closed-open interval, so represents `x` such that `ivStart <= x <
 - ivPast`.
 -
 - The simplest map is the NaiveIntervalInt, which is just a vector. It is very
 - memory efficient, but needs O(N) to search. However, for small N, it is
 - likely very efficient and we can use this "structure" for testing too.
 -
 - The Tree is very simple: At each node, there is a split value and intervals
 - are completely below it, completely above it, or contain the point.
 -
 -
 - Leafs contain NaiveIntervalInt
 -}

data IntervalValue = IntervalValue
                        { ivStart :: !Word32
                        , ivPast :: !Word32
                        , ivValue :: !Word32
                        }
#ifdef IS_BUILDING_TEST
                              deriving (Show)
#endif

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
                            { _nodeSplitValue :: !Int
                            , _leftSplit :: !IntervalIntMapNode
                            , _centerSplit :: !IntervalIntMapNode
                            , _rightSplit :: !IntervalIntMapNode
                            }
#ifdef IS_BUILDING_TEST
                              deriving (Show)
#endif

newtype IntervalIntMap = IntervalIntMap { _imapRoot :: IntervalIntMapNode }

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


sortedByEnd :: NaiveIntervalInt -> NaiveIntervalInt
sortedByEnd vec = VS.create $ do
    vec' <- VS.thaw vec
    sortBy (comparing ivPast) vec'
    return vec'

mkTree :: Int -> NaiveIntervalInt -> IntervalIntMap
mkTree maxSplit vec = IntervalIntMap $ mkTree' 0 maxSplit (sortedByEnd vec)

maxSplitIters :: Int
maxSplitIters = 8

mkTree' nIters maxSplit vec
    | VS.length vec <= maxSplit = Leaf vec
    | nIters > maxSplitIters = Leaf vec
    | otherwise = trySplit nIters maxSplit vec

trySplit nIters maxSplit vec = InnerNode (fromEnum p) (r left) (r center) (r right)
    where
        r = mkTree' nIters' maxSplit
        (left, center, right) = partition (fromEnum p) vec
        nIters'
            | successful = 0
            | otherwise = nIters + 1
        successful = VS.length vec - maximum (map VS.length [left, center, right]) >= maxSplit
        -- Choosing a pivot will probably have a big impact on the performance.
        -- We pick the median end-point one, which is probably a decent impact
        p = ivPast $ (VS.!) vec (VS.length vec `div` 2)

intervalMapFind :: Int -> IntervalIntMap -> [Int]
intervalMapFind x (IntervalIntMap root) = intervalMapFind' root
    where

        intervalMapFind' (Leaf vec) = naiveIntervalMapFind x vec
        intervalMapFind' (InnerNode p left center right)
            | x < p = intervalMapFind' left ++ intervalMapFind' center
            | x == p = intervalMapFind' center
            | otherwise = intervalMapFind' center ++ intervalMapFind' right

naiveIntervalMapFind :: Int -> NaiveIntervalInt -> [Int]
naiveIntervalMapFind x = VS.toList . VS.map (fromEnum . ivValue) . VS.filter (intervalContains x)

