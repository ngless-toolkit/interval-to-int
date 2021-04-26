{-# LANGUAGE FlexibleContexts, TypeApplications #-}

module Data.IntervalIntMap.Internal.IntervalIntIntMap
    ( IntervalValue(..)
    , Interval(..)
    , IntervalIntMap
    , naiveIntervalMapLookup
    , lookup
    , overlaps
    , overlapsWithKeys
    , naiveOverlaps
    , naiveOverlapsWithKeys
    , NaiveIntervalInt
    , intervalContains
    , partition
    , freeze
#ifdef IS_BUILDING_TEST
    , mkTree
#endif
    ) where
import Prelude hiding (lookup)

import qualified Data.IntervalIntMap.Internal.GrowableVector as GV

import qualified Foreign.Storable as FS
import           Foreign.Ptr (castPtr, plusPtr)
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Vector.Storable as VS
import           Control.Monad.ST (runST)
import           Data.Word (Word32)
import           Data.Ord (comparing)
import           Data.Vector.Algorithms.Tim (sortBy)
import           Control.DeepSeq (NFData(..))


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


data Interval = Interval !Int !Int
#ifdef IS_BUILDING_TEST
                            deriving (Eq, Show)
#endif

data IntervalValue = IntervalValue
                        { ivStart :: !Word32
                        , ivPast :: !Word32
                        , ivValue :: !Word32
                        }
#ifdef IS_BUILDING_TEST
                              deriving (Show)
#endif

instance Eq IntervalValue where
    (IntervalValue s0 e0 ix0) == (IntervalValue s1 e1 ix1) =
            s0 == s1 && e0 == e1 && ix0 == ix1

-- This is necessary to build sets of 'IntervalValue's (e.g., in 'naiveOverlapsWithKeys')
instance Ord IntervalValue where
    (IntervalValue s0 e0 ix0) `compare` (IntervalValue s1 e1 ix1)
        | s0 /= s1 = s0 `compare` s1
        | e0 /= e1 = e0 `compare` e1
        | otherwise = ix0 `compare` ix1

instance FS.Storable IntervalValue where
    sizeOf _ = 3 * 4 -- aka 12
    alignment x = FS.alignment (ivStart x)
    peek p = IntervalValue
                    <$> FS.peek (castPtr p)
                    <*> FS.peek (castPtr p `plusPtr` 4)
                    <*> FS.peek (castPtr p `plusPtr` 8)
    poke ptr (IntervalValue s p v) = do
        let ptr' = castPtr ptr
        FS.pokeElemOff @Word32 ptr' 0 s
        FS.pokeElemOff @Word32 ptr' 1 p
        FS.pokeElemOff @Word32 ptr' 2 v

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

instance NFData IntervalIntMapNode where
    rnf (Leaf v) = rnf v
    rnf (InnerNode !_ left center right) = rnf left `seq` rnf center `seq` rnf right

newtype IntervalIntMap = IntervalIntMap { _imapRoot :: IntervalIntMapNode }
#ifdef IS_BUILDING_TEST
                              deriving (Show)
#endif

instance NFData IntervalIntMap where
    rnf (IntervalIntMap !n) = rnf n

partition :: Int -> NaiveIntervalInt -> (NaiveIntervalInt, NaiveIntervalInt, NaiveIntervalInt)
partition p vec = runST $ do
    left <- GV.new
    center <- GV.new
    right <- GV.new
    VS.forM_ vec $ \val ->
        let target
                | ivPast val <= toEnum p = left
                | ivStart val > toEnum p = right
                | otherwise = center
        in GV.pushBack val target
    (,,)
        <$> GV.unsafeFreeze left
        <*> GV.unsafeFreeze center
        <*> GV.unsafeFreeze right


sortedByEnd :: NaiveIntervalInt -> NaiveIntervalInt
sortedByEnd vec = VS.create $ do
    vec' <- VS.thaw vec
    sortBy (comparing ivPast) vec'
    return vec'

{-|
  Turn a 'NaiveIntervalInt' into an 'IntervalIntMap'
-}
freeze :: NaiveIntervalInt -> IntervalIntMap
freeze = mkTree 16

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

        -- The criterion for calling it a successful split is a bit random, but seems to work:
        -- If after splitting the largest component is at least maxSplit
        -- smaller than the input, that was a successful split
        successful = VS.length vec - maximum (map VS.length [left, center, right]) >= maxSplit

        -- Choosing a pivot will probably have a big impact on the performance.
        -- We pick the median end-point one, which is probably a decent heuristic
        p = ivPast $ (VS.!) vec (VS.length vec `div` 2)

lookup :: Int -> IntervalIntMap -> IS.IntSet
lookup x (IntervalIntMap root) = lookup' root
    where

        lookup' (Leaf vec) = naiveIntervalMapLookup x vec
        lookup' (InnerNode p left center right)
            | x < p = lookup' left `IS.union` lookup' center
            | x == p = lookup' center
            | otherwise = lookup' center `IS.union` lookup' right

naiveIntervalMapLookup :: Int -> NaiveIntervalInt -> IS.IntSet
naiveIntervalMapLookup x = IS.fromList . VS.toList . VS.map (fromEnum . ivValue) . VS.filter (intervalContains x)

naiveOverlaps :: Interval -> NaiveIntervalInt -> IS.IntSet
naiveOverlaps i = IS.fromList . map snd . naiveOverlapsWithKeys i

naiveOverlapsWithKeys :: Interval -> NaiveIntervalInt -> [(Interval, Int)]
naiveOverlapsWithKeys i = map asPair . S.toList . naiveOverlapsWithKeys' i

asPair (IntervalValue s e ix) = (Interval (fromEnum s) (fromEnum e), fromEnum ix)

naiveOverlapsWithKeys' :: Interval -> NaiveIntervalInt -> S.Set IntervalValue
naiveOverlapsWithKeys' (Interval s0 e0) = S.fromList . VS.toList . VS.filter overlap1
    where
        overlap1 (IntervalValue s1' e1' _)
            | s0 == e0 = False
            | s1' == e1' = False
            | otherwise =
                let
                    s1 = fromEnum s1'
                    e1 = fromEnum e1'
                in (s0 <= s1 && s1 < e0) || (s1 <= s0 && s0 < e1)

overlaps :: Interval -> IntervalIntMap -> IS.IntSet
overlaps i (IntervalIntMap root) = overlaps' i root

overlaps' i (Leaf vec) = naiveOverlaps i vec
overlaps' i (InnerNode p left centre right)
    | i `intervalAbove` p = overlaps'  i right `IS.union` overlaps' i centre
    | i `intervalBelow` p = overlaps' i left `IS.union` overlaps' i centre
    | otherwise = overlaps' i left `IS.union` overlaps' i centre `IS.union` overlaps' i right

overlapsWithKeys :: Interval -> IntervalIntMap -> [(Interval, Int)]
overlapsWithKeys i (IntervalIntMap root) = map asPair . S.toList $ overlapsWithKeys' i root

overlapsWithKeys' :: Interval -> IntervalIntMapNode -> S.Set IntervalValue
overlapsWithKeys' i (Leaf vec) = naiveOverlapsWithKeys' i vec
overlapsWithKeys' i (InnerNode p left centre right)
    | i `intervalAbove` p = overlapsWithKeys'  i right `S.union` overlapsWithKeys' i centre
    | i `intervalBelow` p = overlapsWithKeys' i left `S.union` overlapsWithKeys' i centre
    | otherwise = overlapsWithKeys' i left `S.union` overlapsWithKeys' i centre `S.union` overlapsWithKeys' i right

intervalAbove (Interval s _) p = s > p
intervalBelow (Interval _ e) p = e <= p

