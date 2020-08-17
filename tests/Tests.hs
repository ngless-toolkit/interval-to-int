{- Copyright 2020 Luis Pedro Coelho
 - License: MIT
 -}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
module Main where

import Test.Tasty.HUnit
import Test.Tasty.TH (defaultMainGenerator)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog



import qualified Data.Vector.Storable as VS
import qualified Data.IntervalIntMap.IntervalIntMap as IM
import           Data.Foldable (for_)
import qualified Data.IntSet as IS


tData =
    [ IM.IntervalValue 0  2 0
    , IM.IntervalValue 0  2 1
    , IM.IntervalValue 1  2 2
    , IM.IntervalValue 3  6 3
    , IM.IntervalValue 3  4 4
    , IM.IntervalValue 1  4 5
    , IM.IntervalValue 4  7 6
    , IM.IntervalValue 4  6 7
    , IM.IntervalValue 8 10 8
    , IM.IntervalValue 1 12 9
    ]

tDataN :: IM.NaiveIntervalInt
tDataN = VS.fromList tData

below x (IM.IntervalValue _ e _) = x >= e
above x (IM.IntervalValue s _ _) = x < s

case_partition =
    for_ [0..14] $ \split -> do
        let (left,center,right) = IM.partition split tDataN
        all (below $ toEnum split) (VS.toList left) @? "Center does not include split"
        all (IM.intervalContains $ toEnum split) (VS.toList center) @? "Left is not below split"
        all (above $ toEnum split) (VS.toList right) @? "Right is not above split"
        VS.length left + VS.length center + VS.length right @=? VS.length tDataN

case_small_build_tree_find = do
    let t = IM.mkTree 4 tDataN
    for_ [0..14] $ \x ->
        IM.lookup x t @=? IM.naiveIntervalMapLookup x tDataN

genSimpleInterval space = do
    s <- Gen.integral (Range.linear 0 space)
    len <- Gen.integral (Range.linear 0 space)
    return (s, s + len)

prop_build_tree_find = H.property $ do
    -- the smaller values will generate more crowded inputs
    space <- H.forAll $ Gen.integral (Range.linear 100 2000)
    intervals <- H.forAll $ Gen.list (Range.linear 0 2000) $ genSimpleInterval space
    ps <- H.forAll $ Gen.list (Range.linear 0 5) $ Gen.integral $ Range.linear 0 10000
    H.classify "empty" $ length intervals == 0
    H.classify "small (N< 100)" $ length intervals < 100
    H.classify "large (N>=100)" $ length intervals >= 100
    let naive = VS.fromList [IM.IntervalValue s e ix | ((s,e),ix) <- zip intervals [0..]]
        t = IM.mkTree 16 naive
    for_ ps $ \p ->
        IM.lookup p t H.=== IM.naiveIntervalMapLookup p naive



prop_naive_overlaps1 = H.property $ do
    (s,e) <- H.forAll $ genSimpleInterval 100
    (s',e') <- H.forAll $ genSimpleInterval 100
    let naive = VS.fromList [IM.IntervalValue (toEnum s) (toEnum e) 0]
        i = IM.Interval s' e'
        doesOverlap
            | s >= e = False
            | otherwise = any (\c -> s' <= c && c < e') [s..(e-1)]
    H.classify "empty interval" $ s == e || s' == e'
    H.classify "overlaps" $ doesOverlap
    H.classify "no overlap" $ not doesOverlap
    (not . IS.null $ IM.naiveOverlaps i naive) H.=== doesOverlap

main :: IO ()
main = $(defaultMainGenerator)


