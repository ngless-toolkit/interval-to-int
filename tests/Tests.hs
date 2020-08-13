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
import           Data.IntervalIntMap.IntervalIntMap
import           Data.Foldable (for_)
import qualified Data.IntSet as IS


tData =
    [ IntervalValue 0  2 0
    , IntervalValue 0  2 1
    , IntervalValue 1  2 2
    , IntervalValue 3  6 3
    , IntervalValue 3  4 4
    , IntervalValue 1  4 5
    , IntervalValue 4  7 6
    , IntervalValue 4  6 7
    , IntervalValue 8 10 8
    , IntervalValue 1 12 9
    ]

tDataN :: NaiveIntervalInt
tDataN = VS.fromList tData

below x (IntervalValue _ e _) = x >= e
above x (IntervalValue s _ _) = x < s

case_partition =
    for_ [0..14] $ \split -> do
        let (left,center,right) = partition split tDataN
        all (below $ toEnum split) (VS.toList left) @? "Center does not include split"
        all (intervalContains $ toEnum split) (VS.toList center) @? "Left is not below split"
        all (above $ toEnum split) (VS.toList right) @? "Right is not above split"
        VS.length left + VS.length center + VS.length right @=? VS.length tDataN

case_small_build_tree_find = do
    let t = mkTree 4 tDataN
    for_ [0..14] $ \x ->
        IS.fromList (intervalMapFind x t) @=? IS.fromList (naiveIntervalMapFind x tDataN)

prop_build_tree_find = H.property $ do
    -- the smaller values will generate more crowded inputs
    space <- H.forAll $ Gen.integral (Range.linear 100 2000)
    intervals <- H.forAll $ Gen.list (Range.linear 0 2000) $ do
                                s <- Gen.integral (Range.linear 0 space)
                                ell <- Gen.integral (Range.linear 0 space)
                                return (s, s + ell)
    ps <- H.forAll $ Gen.list (Range.linear 0 5) $ Gen.integral $ Range.linear 0 10000
    H.classify "empty" $ length intervals == 0
    H.classify "small (N< 100)" $ length intervals < 100
    H.classify "large (N>=100)" $ length intervals >= 100
    let naive = VS.fromList [IntervalValue s e ix | ((s,e),ix) <- zip intervals [0..]]
        t = mkTree 16 naive
    for_ ps $ \p ->
        IS.fromList (intervalMapFind p t) H.===
            IS.fromList (naiveIntervalMapFind p naive)


main :: IO ()
main = $(defaultMainGenerator)


