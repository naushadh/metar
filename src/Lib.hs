module Lib
  (-- * Types
    Metar(..)
  , ICAO(..)
  , Timestamp(..)
  , Wind(..)
  , Speed(..)
  , UnitOfSpeed(..)
  -- * Parser(s)
  , parseMetar
  -- * Printer(s)
  , renderMetar
  -- * Reporting
  , MapAirportSpeed
  , pipeline
  , printReport
  , seedToFile
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
import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BSC

import qualified Test.QuickCheck.Arbitrary as Arbitrary
import qualified Test.QuickCheck.Gen as Gen
import           Test.QuickCheck.Instances.Time () -- instances for UTCTime
import qualified Data.List as L
import           Control.Monad.IO.Class (liftIO)

--------------------------------------------------------------------------------
-- * Metar

-- | A line from a METAR report
data Metar = Metar
  { metarICAO :: ICAO
  , metarTimestamp :: Timestamp
  , metarWind :: Wind
  } deriving Show

instance Eq Metar where
  a == b = renderMetar a == renderMetar b

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

--------------------------------------------------------------------------------
-- * ICAO

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

--------------------------------------------------------------------------------
-- * Timestamp

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

--------------------------------------------------------------------------------
-- * Wind

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
 ++ Maybe.maybe "" (\g' -> "G" ++ leftpad 2 (show g')) g
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

-- | getter for 'Speed' magnitude
speedValue :: Speed -> Natural
speedValue (Speed _ v) = v

-- | Normalize a speed to MPS
speedToMPS :: Speed -> Speed
speedToMPS (Speed u v) = Speed MPS (go u v)
  where
    go KT n = n `div` 2
    go MPS n = n

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

--------------------------------------------------------------------------------
-- * Reporting

-- | Acquire the last N speeds for every airport
-- NOTE: we assume provided FilePath exists & is readable!
pipeline :: FilePath -> Int -> IO MapAirportSpeed
pipeline srcFP limit
  = C.runConduitRes
   $ CC.sourceFile srcFP -- TODO: add ability to provide pretty error messaging should this fail.
  .| CC.linesUnboundedAscii
  .| CC.map BSC.unpack
  .| CC.map (M.runParser parseMetar "")
  .| takeRights -- TODO: add ability to somehow report on the # and/or lines of bad records.
  .| CC.map (\m -> m { metarWind = (metarWind m) { windSpeed = speedToMPS . windSpeed . metarWind $ m } } )
  .| CC.foldl (boundAppend limit) mempty

-- | just extract the right hand side
takeRights :: Monad m => C.ConduitT (Either l r) r m ()
takeRights = CC.filter Either.isRight .| CC.map (Either.fromRight (error "unexpected left!"))

-- | Last N speeds per airport
type MapAirportSpeed = Map.Map Text [Natural]

-- | Append a Metar speed into the ICAO key, evict an older speed when limit is reached.
boundAppend :: Int -> MapAirportSpeed -> Metar -> MapAirportSpeed
boundAppend limit as m = Map.adjust go k (Map.insertWith (++) k mempty as)
  where
    k = unICAO . metarICAO $ m
    v = speedValue . windSpeed . metarWind $ m
    ms = Map.findWithDefault mempty k as
    append = (++ [v])
    go = if length ms <= limit then append else (append . tail)

-- | average windspeed for an Airport
averageSpeed :: [Natural] -> Float
averageSpeed xs = (fromIntegral $ sum xs) / (fromIntegral $ length xs)

-- | current windspeed for an Airport
-- NOTE: we rely on 'boundAppend' to ensure no empty keys are populated.
currentSpeed :: [Natural] -> Natural
currentSpeed = last

-- | Print a report of the running average and current speeds per airport.
printReport :: MapAirportSpeed -> IO ()
printReport xs = header >> mapM_ (uncurry go) (Map.toList xs)
  where
    header = putStrLn "Airport,Average,Current"
    go a ss
      = putStrLn $ (T.unpack a)
      ++ ","
      ++ (show $ averageSpeed ss)
      ++ ","
      ++ (show $ currentSpeed ss)

--------------------------------------------------------------------------------
-- * Utils

-- | Possible errors we can emit
data Error
  = InvalidLine Natural Text -- ^ error parsing a line, line# and error message.
  deriving Show

-- | Our humble string parser
type Parser = M.Parsec Void String

-- | Left pad a string with 0s
-- Lifted from: https://stackoverflow.com/a/29153602
leftpad :: Int -> String -> String
leftpad m xs = replicate (m - length ys) '0' ++ ys
  where ys = take m xs

--------------------------------------------------------------------------------
-- * Artbitrary instances for test data generation

-- | Seed test data generated via quick-check into a file.
seedToFile :: FilePath -> Int -> IO ()
seedToFile destFP limit
  = C.runConduitRes
   $ CL.sourceList [1..chunks]
  .| CC.mapM (const $ liftIO mkMetars)
  .| CC.map (\ms -> L.intercalate "\n" $ fmap renderMetar ms)
  .| CC.map BSC.pack
  .| CC.sinkFile destFP -- TODO: add pretty error message should writing to file fail.
  where
    chunkSize = 10000
    chunks = (limit `div` chunkSize)+1
    mkMetars :: IO [Metar]
    mkMetars = Gen.generate $ Gen.vectorOf chunkSize Arbitrary.arbitrary

instance Arbitrary.Arbitrary Metar where
  arbitrary
    = Metar
    <$> Arbitrary.arbitrary
    <*> Arbitrary.arbitrary
    <*> Arbitrary.arbitrary

instance Arbitrary.Arbitrary ICAO where
  arbitrary = ICAO . T.pack <$> (listOf $ Gen.elements ['A'..'Z'])
    where
      -- limited bounds to ensure test data has repeated names to verify averaging.
      listOf a = Gen.choose (3,5) >>= \i -> Gen.vectorOf i a

instance Arbitrary.Arbitrary Timestamp where
  arbitrary = Timestamp <$> Arbitrary.arbitrary

instance Arbitrary.Arbitrary Wind where
  arbitrary
    = Wind
    <$> Arbitrary.arbitrarySizedNatural
    <*> Arbitrary.arbitrary
    <*> Arbitrary.liftArbitrary Arbitrary.arbitrarySizedNatural

instance Arbitrary.Arbitrary Speed where
  arbitrary
    = Speed
    <$> Arbitrary.arbitrary
    <*> Arbitrary.arbitrarySizedNatural

instance Arbitrary.Arbitrary UnitOfSpeed where
  arbitrary = Gen.elements [ KT, MPS ]
