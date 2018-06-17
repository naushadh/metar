module Main (main) where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Lib
import qualified Text.Megaparsec as M

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
