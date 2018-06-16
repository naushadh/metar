module Lib
  ( someFunc
  -- * Types
  , ICAO(..)
  , Timestamp(..)
  , Wind(..)
  ) where

import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Numeric.Natural (Natural)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | International Civil Aviation Code.
-- Ex: YYZ (for Toronto), LAX (Los Angeles), etc.
newtype ICAO = ICAO { unICAO :: Text }
  deriving Show

-- | Simple wrapper over 'UTCTime' to allow adding custom parsing instances.
-- NOTE: the year within is defaulted to the current year, the icoming feed DOES NOT tell us this.
-- NOTE: we assume incoming data is UTC time-zoned.
-- Feel free to ignore the timezone and extract the date/time to a desired local time.
newtype Timestamp = Timestamp { unTimestamp :: UTCTime }
  deriving Show

-- | Wind info
data Wind = Wind
  { windDirection :: Natural  -- ^ in degrees
  , windSpeed :: Natural      -- ^ speed in MPH
  , windGusts :: Maybe Natural-- ^ gusts (if any)
  } deriving Show
