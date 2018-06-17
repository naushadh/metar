module Lib
  ( someFunc
  -- * Types
  , Metar(..)
  , ICAO(..)
  , Timestamp(..)
  , Wind(..)
  -- * Parser(s)
  , parseMetar
  ) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Time (UTCTime)
import qualified Data.Time as Time
import           Numeric.Natural (Natural)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Control.Monad.Combinators as ParserCombinators
import           Data.Void (Void)
import           Control.Applicative ((<|>), optional)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--------------------------------------------------------------------------------
-- * Types

-- | Our humble string parser
type Parser = M.Parsec Void String

-- | A line from a METAR report
data Metar = Metar
  { metarICAO :: ICAO
  , metarTimestamp :: Timestamp
  , metarWind :: Wind
  } deriving Show

-- | Parse a 'Metar'
parseMetar :: Parser Metar
parseMetar = Metar <$> parseICAO <*> (parseSpace >> parseTimestamp) <*> (parseSpace >> parseWind)
  where
    parseSpace :: Parser Char
    parseSpace = MC.char ' '

-- | International Civil Aviation Code.
-- Ex: YYZ (for Toronto), LAX (Los Angeles), etc.
newtype ICAO = ICAO { unICAO :: Text }
  deriving Show

-- | Parse an 'ICAO'
parseICAO :: Parser ICAO
parseICAO = ICAO . T.pack <$> ParserCombinators.some MC.upperChar

-- | Simple wrapper over 'UTCTime' to allow adding custom parsing instances.
-- NOTE: the year within is defaulted to the current year, the incoming feed DOES NOT tell us this.
-- NOTE: we assume incoming data is UTC time-zoned.
-- Feel free to ignore the timezone and extract the date/time to a desired local time.
newtype Timestamp = Timestamp { unTimestamp :: UTCTime }
  deriving Show

-- | Parse a 'Timestamp'
parseTimestamp :: Parser Timestamp
parseTimestamp
  = Timestamp <$> parseUTCTime "%d%H%MZ"
  where
    parseUTCTime :: String -> Parser UTCTime
    parseUTCTime pattern
      =   Time.parseTimeM False Time.defaultTimeLocale pattern
      =<< ParserCombinators.count (length pattern) MC.asciiChar

-- | Wind info
data Wind = Wind
  { windDirection :: Natural  -- ^ in degrees
  , windSpeed :: Natural      -- ^ speed in MPS
  , windGusts :: Maybe Natural-- ^ gusts (if any)
  } deriving Show

-- | Parse a 'Wind'
parseWind :: Parser Wind
parseWind = go <$> parseLine
  where
    parseNatural :: Int -> Parser Natural
    parseNatural n = read <$> ParserCombinators.count n MC.digitChar
    parseNatural' :: Int -> Int -> Parser Natural
    parseNatural' n1 n2 = read <$> ParserCombinators.count' n1 n2 MC.digitChar
    parseGusts = MC.char 'G' >> parseNatural 2
    parseLine
      = ((,,,) <$> parseNatural 3 <*> (parseNatural' 2 3) <*> optional parseGusts <*> parseUnitOfSpeed)
    go (d,s,g,u) = Wind d (speedToMPS u s) g

-- | Unit of speed
data UnitOfSpeed = Knot | MPS
  deriving Show

-- | Parse a 'UnitOfSpeed'
parseUnitOfSpeed :: Parser UnitOfSpeed
parseUnitOfSpeed
  =  (MC.string "KT" >> pure Knot)
 <|> (MC.string "MPS" >> pure MPS)

-- | Normalize a speed to MPS
speedToMPS :: UnitOfSpeed -> Natural -> Natural
speedToMPS Knot n = n `div` 2
speedToMPS MPS n = n

-- | Possible errors we can emit
data Error
  = InvalidLine Natural Text -- ^ error parsing a line, line# and error message.
  deriving Show
