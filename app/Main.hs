module Main where

import qualified Lib
import qualified Options.Applicative as Opt
import           Data.Monoid ((<>))
import           Control.Applicative ((<|>))

main :: IO ()
main = run =<< Opt.execParser opts

run :: Options -> IO ()
run (SeedFile fp limit) = Lib.seedToFile fp limit
run (Report fp rollingWindow) = Lib.pipeline fp rollingWindow >>= Lib.printReport

data Options
  = SeedFile FilePath Int
  | Report FilePath Int

opts :: Opt.ParserInfo Options
opts
  = Opt.info (Opt.helper <*> options)
      ( Opt.fullDesc
     <> Opt.progDesc "A CLI for METAR"
     <> Opt.header "metar - parse or generate reports" )

options :: Opt.Parser Options
options
  = Report
    <$> Opt.strOption
        ( Opt.long "in"
        <> Opt.metavar "FILEPATH"
        <> Opt.help "Metar file to parse and report on"
        )
    <*> Opt.option Opt.auto
        ( Opt.long "rolling_average"
        <> Opt.metavar "INT"
        <> Opt.help "The number of reports to account for in the rolling average"
        <> Opt.showDefault
        <> Opt.value 10
        )
  <|> SeedFile
    <$> Opt.strOption
        ( Opt.long "out"
        <> Opt.metavar "FILEPATH"
        <> Opt.help "Filepath to dump randomly generated data into"
        )
    <*> Opt.option Opt.auto
        ( Opt.long "limit"
        <> Opt.metavar "INT"
        <> Opt.help "The number of reports to generate"
        <> Opt.showDefault
        <> Opt.value 100000
        )
