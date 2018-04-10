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
  Gen.list (linear 0 40) Gen.unicode

alpha100 ::
  Gen String
alpha100 =
  Gen.list (linear 0 40) Gen.alpha

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
      cl <- Gen.list (linear 0 40) unicode100
      kv <- Gen.list (linear 0 40) ((,) <$> unicode100 <*> unicode100)
      pure (it, cl, kv)

genFormat ::
  Gen Format
genFormat =
  Definition.Format <$> unicode100 

genInlines ::
  Range Int
  -> Gen [Inline]
genInlines r =
  Gen.list r (Gen.small genInline)

genInline ::
  Gen Inline
genInline =
  Gen.choice
    [
      Str <$> unicode100
    , Emph <$> genInlines (linear 0 40)
    , Strong <$> genInlines (linear 0 40)
    , Strikeout <$> genInlines (linear 0 40)
    , Superscript <$> genInlines (linear 0 40)
    , Subscript <$> genInlines (linear 0 40)
    , SmallCaps <$> genInlines (linear 0 40)
    , Quoted <$> genQuoteType <*> genInlines (linear 0 40)
    , Cite <$> genCitations (linear 0 40) <*> genInlines (linear 0 40)
    , Code <$> genAttr <*> unicode100
    , pure Definition.Space
    , pure SoftBreak
    , pure LineBreak
    , Math <$> genMathType <*> unicode100
    , RawInline <$> genFormat <*> unicode100 
    , Link <$> genAttr <*> genInlines (linear 0 40) <*> genTarget 
    , Image <$> genAttr <*> genInlines (linear 0 40) <*> genTarget 
    , Note <$> genBlocks (linear 0 40)
    , Span <$> genAttr <*> genInlines (linear 0 40)
    ]
   
genBlocks ::
  Range Int
  -> Gen [Block]
genBlocks r =
  Gen.list r (Gen.small genBlock)

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
    Gen.int (linear 0 40) <*>
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
      Plain <$> genInlines (linear 0 40)
    , Para <$> genInlines (linear 0 40)
    , LineBlock <$> Gen.list (linear 0 40) (genInlines (linear 0 40))
    , CodeBlock <$> genAttr <*> unicode100
    , RawBlock <$> genFormat <*> unicode100
    , BlockQuote <$> genBlocks (linear 0 40)
    , OrderedList <$> genListAttributes <*> Gen.list (linear 0 40) (genBlocks (linear 0 40))
    , BulletList <$> Gen.list (linear 0 40) (genBlocks (linear 0 40))
    , DefinitionList <$> Gen.list (linear 0 40) ((,) <$> genInlines (linear 0 40) <*> Gen.list (linear 0 40) (genBlocks (linear 0 40)))
    , Header <$> Gen.int (linear 0 40) <*> genAttr <*> genInlines (linear 0 40)
    , pure HorizontalRule
    , Table <$> genInlines (linear 0 40) <*> genAlignments (linear 0 40) <*> Gen.list (linear 0 40) (Gen.double (linearFrac 0 100)) <*> Gen.list (linear 0 40) (genBlocks (linear 0 40)) <*> Gen.list (linear 0 40) (Gen.list (linear 0 40) (genBlocks (linear 0 40)))
    , Div <$> genAttr <*> genBlocks (linear 0 40)
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
    genInlines (linear 0 40) <*>
    genInlines (linear 0 40) <*>
    genCitationMode <*>
    Gen.int (linear 0 40) <*>
    Gen.int (linear 0 40)

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
  Gen.list r (Gen.small genMetaValue)

genMetaValue ::
  Gen MetaValue
genMetaValue =
  Gen.choice
    [
      MetaBool <$> Gen.bool
    , MetaString <$> alpha100
    , MetaMap <$> genMapMetaValue
    , MetaList <$> genMetaValues (linear 0 40)
    , MetaInlines <$> genInlines (linear 0 40)
    , MetaBlocks <$> genBlocks (linear 0 40)
    ]

genMapMetaValue ::
  Gen (Map String MetaValue)
genMapMetaValue =
  Gen.map (linear 0 40) $
    do  k <- Gen.frequency
              [
                (1, pure "title")
              , (1, pure "author")
              , (1, pure "date")
              , (97, alpha100)
              ]
        v <- Gen.small genMetaValue
        pure (k, v)

genMeta ::
  Gen Meta
genMeta =
  Meta <$> genMapMetaValue

genPandoc ::
  Gen Pandoc
genPandoc =
  Pandoc <$> genMeta <*> genBlocks (linear 0 40)
