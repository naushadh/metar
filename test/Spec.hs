{-# OPTIONS_GHC -fno-warn-orphans #-} -- we need this to derive Arbitrary instances.

module Main (main) where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Arbitrary as Arbitrary
import qualified Test.QuickCheck.Gen as Gen
import qualified Lib
import qualified Data.Text as T
import qualified Text.Megaparsec as M
import           Test.QuickCheck.Instances.Time () -- instances for UTCTime

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Tests" [properties]

properties :: Tasty.TestTree
properties = Tasty.testGroup "Properties" [qcProps]

qcProps :: Tasty.TestTree
qcProps = Tasty.testGroup "(checked by QuickCheck)"
  [ QC.testProperty "parseMetar can parse arbitrary 'Metar'" $
      \x -> (M.runParser Lib.parseMetar "" $ Lib.renderMetar x) == Right (x :: Lib.Metar)
  ]

--------------------------------------------------------------------------------
-- * Orphans

instance Arbitrary.Arbitrary Lib.Metar where
  arbitrary
    = Lib.Metar
    <$> Arbitrary.arbitrary
    <*> Arbitrary.arbitrary
    <*> Arbitrary.arbitrary

instance Arbitrary.Arbitrary Lib.ICAO where
  arbitrary = Lib.ICAO . T.pack <$> (Gen.listOf1 $ Gen.elements ['A'..'Z'])

instance Arbitrary.Arbitrary Lib.Timestamp where
  arbitrary = Lib.Timestamp <$> Arbitrary.arbitrary

instance Arbitrary.Arbitrary Lib.Wind where
  arbitrary
    = Lib.Wind
    <$> Arbitrary.arbitrarySizedNatural
    <*> Arbitrary.arbitrary
    <*> Arbitrary.liftArbitrary Arbitrary.arbitrarySizedNatural

instance Arbitrary.Arbitrary Lib.Speed where
  arbitrary
    = Lib.Speed
    <$> Arbitrary.arbitrary
    <*> Arbitrary.arbitrarySizedNatural

instance Arbitrary.Arbitrary Lib.UnitOfSpeed where
  arbitrary = Gen.elements [ Lib.KT, Lib.MPS ]
