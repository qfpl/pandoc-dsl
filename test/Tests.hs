{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Map
import Hedgehog
import Hedgehog.Range
import qualified Hedgehog.Gen as Gen
import Papa
import Test.Tasty(defaultMain, testGroup)
import Test.Tasty.Hedgehog
import Text.Pandoc.Definition
import qualified Text.Pandoc.Definition as Definition

main ::
  IO ()
main =
  defaultMain $ testGroup "Tests" [
    testGroup "Pandoc" [
      testProperty "y" prop_reverse
    ]
  ]

prop_reverse ::
  Property
prop_reverse =
  property $ do
    xs <- forAll $ alpha100
    reverse (reverse xs) === xs

unicode100 ::
  Gen String
unicode100 =
  Gen.list (linear 0 100) Gen.unicode

alpha100 ::
  Gen String
alpha100 =
  Gen.list (linear 0 100) Gen.alpha

genQuoteType ::
  Gen QuoteType
genQuoteType =
  Gen.element
    [
      SingleQuote
    , DoubleQuote
    ]


genMathType ::
  Gen MathType
genMathType =
  Gen.element
    [
      DisplayMath
    , InlineMath
    ]

genTarget ::
  Gen Target
genTarget =
  ((,) <$> unicode100 <*> unicode100)

genAttr ::
  Gen Attr
genAttr =
  do  it <- unicode100
      cl <- Gen.list (linear 0 100) unicode100
      kv <- Gen.list (linear 0 100) ((,) <$> unicode100 <*> unicode100)
      pure (it, cl, kv)

genFormat ::
  Gen Format
genFormat =
  Definition.Format <$> unicode100 

genInlines ::
  Range Int
  -> Gen [Inline]
genInlines r =
  Gen.list r genInline

genInline ::
  Gen Inline
genInline =
  Gen.choice
    [
      Str <$> unicode100
    , Emph <$> genInlines (linear 0 100)
    , Strong <$> genInlines (linear 0 100)
    , Strikeout <$> genInlines (linear 0 100)
    , Superscript <$> genInlines (linear 0 100)
    , Subscript <$> genInlines (linear 0 100)
    , SmallCaps <$> genInlines (linear 0 100)
    , Quoted <$> genQuoteType <*> genInlines (linear 0 100)
    , Cite <$> genCitations (linear 0 100) <*> genInlines (linear 0 100)
    , Code <$> genAttr <*> unicode100
    , pure Definition.Space
    , pure SoftBreak
    , pure LineBreak
    , Math <$> genMathType <*> unicode100
    , RawInline <$> genFormat <*> unicode100 
    , Link <$> genAttr <*> genInlines (linear 0 100) <*> genTarget 
    , Image <$> genAttr <*> genInlines (linear 0 100) <*> genTarget 
    , Note <$> genBlocks (linear 0 100)
    , Span <$> genAttr <*> genInlines (linear 0 100)
    ]
   
genBlocks ::
  Range Int
  -> Gen [Block]
genBlocks r =
  Gen.list r genBlock

genListNumberDelim ::
  Gen ListNumberDelim
genListNumberDelim =
  Gen.element
    [
      DefaultDelim
    , Period
    , OneParen
    , TwoParens    
    ]

genListNumberStyle ::
  Gen ListNumberStyle
genListNumberStyle =
  Gen.element
    [
      DefaultStyle
    , Example
    , Decimal
    , LowerRoman
    , UpperRoman
    , LowerAlpha
    , UpperAlpha
    ]

genListAttributes ::
  Gen ListAttributes
genListAttributes =
  (,,) <$>
    Gen.int (linear 0 100) <*>
    genListNumberStyle <*>
    genListNumberDelim

genAlignments ::
  Range Int
  -> Gen [Alignment]
genAlignments r =
  Gen.list r genAlignment

genAlignment ::
  Gen Alignment
genAlignment =
  Gen.element
    [
      AlignLeft
    , AlignRight
    , AlignCenter
    , AlignDefault
    ]

genBlock ::
  Gen Block
genBlock =
  Gen.choice
    [
      Plain <$> genInlines (linear 0 100)
    , Para <$> genInlines (linear 0 100)
    , LineBlock <$> Gen.list (linear 0 100) (genInlines (linear 0 100))
    , CodeBlock <$> genAttr <*> unicode100
    , RawBlock <$> genFormat <*> unicode100
    , BlockQuote <$> genBlocks (linear 0 100)
    , OrderedList <$> genListAttributes <*> Gen.list (linear 0 100) (genBlocks (linear 0 100))
    , BulletList <$> Gen.list (linear 0 100) (genBlocks (linear 0 100))
    , DefinitionList <$> Gen.list (linear 0 100) ((,) <$> genInlines (linear 0 100) <*> Gen.list (linear 0 100) (genBlocks (linear 0 100)))
    , Header <$> Gen.int (linear 0 100) <*> genAttr <*> genInlines (linear 0 100)
    , pure HorizontalRule
    , Table <$> genInlines (linear 0 100) <*> genAlignments (linear 0 100) <*> Gen.list (linear 0 100) (Gen.double (linearFrac 0 100)) <*> Gen.list (linear 0 100) (genBlocks (linear 0 100)) <*> Gen.list (linear 0 100) (Gen.list (linear 0 100) (genBlocks (linear 0 100)))
    , Div <$> genAttr <*> genBlocks (linear 0 100)
    , pure Null
    ]

genCitations ::
  Range Int
  -> Gen [Citation]
genCitations r =
  Gen.list r genCitation

genCitation ::
  Gen Citation
genCitation =
  Citation <$>
    unicode100 <*>
    genInlines (linear 0 100) <*>
    genInlines (linear 0 100) <*>
    genCitationMode <*>
    Gen.int (linear 0 100) <*>
    Gen.int (linear 0 100)

genCitationMode ::
  Gen CitationMode
genCitationMode =
  Gen.element
    [
      AuthorInText
    , SuppressAuthor
    , NormalCitation
    ]

genMetaValues ::
  Range Int
  -> Gen [MetaValue]
genMetaValues r =
  Gen.list r genMetaValue

genMetaValue ::
  Gen MetaValue
genMetaValue =
  Gen.choice
    [
      MetaBool <$> Gen.bool
    , MetaString <$> alpha100
    , MetaMap <$> genMapMetaValue
    , MetaList <$> genMetaValues (linear 0 100)
    , MetaInlines <$> genInlines (linear 0 100)
    , MetaBlocks <$> genBlocks (linear 0 100)
    ]

genMapMetaValue ::
  Gen (Map String MetaValue)
genMapMetaValue =
  Gen.map (linear 0 100) genMetaPair

genMetaPair ::
  Gen (String, MetaValue)
genMetaPair =
  do  k <- Gen.frequency
              [
                (1, pure "title")
              , (1, pure "author")
              , (1, pure "date")
              , (97, alpha100)
              ]
      v <- genMetaValue
      pure (k, v)

genMeta ::
  Gen Meta
genMeta =
  Meta <$> genMapMetaValue
