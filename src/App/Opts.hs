module App.Opts where

import Control.Monad (when)
import Extended.Text (Text)
import Extended.Text qualified as T
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    helper,
    info,
    long,
    short,
    strOption,
    switch,
    (<**>), help
  )

data Options = Options
  { optConfigPath :: FilePath,
    optRunMigrations :: Bool
  }

configFileParser :: Parser FilePath
configFileParser =
  strOption
    ( long "conf"
   <> help "Configuration file location"
    )

migrationParser :: Parser Bool
migrationParser =
  switch
    ( long "migrate"
        <> short 'm'
   <> help "Run database migrations before running server"
    )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> configFileParser
    <*> migrationParser

runWithOpts :: IO Options
runWithOpts = execParser $ info (optionsParser <**> helper) fullDesc