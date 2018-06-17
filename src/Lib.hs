module Lib
  ( someFunc
  -- * Types
  , Metar(..)
  , ICAO(..)
  , Timestamp(..)
  , Wind(..)
  -- * Parser(s)
  , parseMetar
  -- * Printer(s)
  , renderMetar
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
import qualified Data.Maybe as Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--------------------------------------------------------------------------------
-- * Types + Parsers

-- | Our humble string parser
type Parser = M.Parsec Void String

-- | A line from a METAR report
data Metar = Metar
  { metarICAO :: ICAO
  , metarTimestamp :: Timestamp
  , metarWind :: Wind
  } deriving Show

-- | Render a 'Metar'
renderMetar :: Metar -> String
renderMetar (Metar i t w)
  = renderICAO i
  ++ " "
  ++ renderTimestamp t
  ++ " "
  ++ renderWind w

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

-- | Render an 'ICAO'
renderICAO :: ICAO -> String
renderICAO (ICAO t) = T.unpack t

-- | Parse an 'ICAO'
parseICAO :: Parser ICAO
parseICAO = ICAO . T.pack <$> ParserCombinators.some MC.upperChar

-- | Simple wrapper over 'UTCTime' to allow adding custom parsing instances.
-- NOTE: the year within is defaulted to the current year, the incoming feed DOES NOT tell us this.
-- NOTE: we assume incoming data is UTC time-zoned.
-- Feel free to ignore the timezone and extract the date/time to a desired local time.
newtype Timestamp = Timestamp { unTimestamp :: UTCTime }
  deriving Show

-- | Standard pattern for 'Timestamp'
timestampPattern :: String
timestampPattern = "%d%H%MZ"

-- | Render a 'Timestamp'
renderTimestamp :: Timestamp -> String
renderTimestamp (Timestamp t) = Time.formatTime Time.defaultTimeLocale timestampPattern t

-- | Parse a 'Timestamp'
parseTimestamp :: Parser Timestamp
parseTimestamp
  = Timestamp <$> parseUTCTime timestampPattern
  where
    parseUTCTime :: String -> Parser UTCTime
    parseUTCTime pattern
      =   Time.parseTimeM False Time.defaultTimeLocale pattern
      =<< ParserCombinators.count (length pattern) MC.asciiChar

-- | Wind info
data Wind = Wind
  { windDirection :: Natural  -- ^ in degrees
  , windSpeed :: Speed        -- ^ speed
  , windGusts :: Maybe Natural-- ^ gusts (if any)
  } deriving Show

-- | Render 'Wind' info
renderWind :: Wind -> String
renderWind (Wind d (Speed u v) g)
  = leftpad 3 (show d)
 ++ (if v > 99 then leftpad 3 (show v) else leftpad 2 (show v))
 ++ Maybe.maybe "" show g
 ++ renderUnitOfSpeed u

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
    go (d,s,g,u) = Wind d (Speed u s) g

-- | Speed with a unit
data Speed = Speed UnitOfSpeed Natural
  deriving Show

-- | Normalize a speed to MPS
-- speedToMPS :: Speed -> Speed
-- speedToMPS (Speed u v) = Speed MPS (go u v)
--   where
--     go KT n = n `div` 2
--     go MPS n = n

-- | Unit of speed
data UnitOfSpeed = KT | MPS
  deriving Show

-- | Render a 'UnitOfSpeed'
renderUnitOfSpeed :: UnitOfSpeed -> String
renderUnitOfSpeed = show

-- | Parse a 'UnitOfSpeed'
parseUnitOfSpeed :: Parser UnitOfSpeed
parseUnitOfSpeed
  =  (MC.string "KT" >> pure KT)
 <|> (MC.string "MPS" >> pure MPS)

-- | Possible errors we can emit
data Error
  = InvalidLine Natural Text -- ^ error parsing a line, line# and error message.
  deriving Show

-- | Left pad a string with 0s
-- Lifted from: https://stackoverflow.com/a/29153602
leftpad :: Int -> String -> String
leftpad m xs = replicate (m - length ys) '0' ++ ys
  where ys = take m xs
