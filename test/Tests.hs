{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Map
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Papa
import Test.Tasty(defaultMain, testGroup)
import Test.Tasty.Hedgehog
import Text.Pandoc.Definition

main ::
  IO ()
main =
  defaultMain $ testGroup "Tests" [
    testGroup "Pandoc" [
      testProperty "y" prop_reverse
    ]
  ]

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs

undefined = undefined

genInline ::
  Gen Inline
genInline =
  undefined
   
genBlock ::
  Gen Block
genBlock =
  undefined

genMetaValue ::
  Gen MetaValue
genMetaValue =
  Gen.choice
    [
      MetaBool <$> Gen.bool
    , MetaString <$> Gen.list (Range.linear 0 100) Gen.alpha
    , MetaMap <$> genMapMetaValue
    , MetaList <$> Gen.list (Range.linear 0 100) genMetaValue
    , MetaInlines <$> Gen.list (Range.linear 0 100) genInline
    , MetaBlocks <$> Gen.list (Range.linear 0 100) genBlock
    ]

genMapMetaValue ::
  Gen (Map String MetaValue)
genMapMetaValue =
  Gen.map (Range.linear 0 100) genMetaPair

genMetaPair ::
  Gen (String, MetaValue)
genMetaPair =
  do  k <- Gen.frequency
              [
                (1, pure "title")
              , (1, pure "author")
              , (1, pure "date")
              , (97, Gen.list (Range.linear 0 100) Gen.alpha)
              ]
      v <- genMetaValue
      pure (k, v)

genMeta ::
  Gen Meta
genMeta =
  Meta <$> genMapMetaValue
