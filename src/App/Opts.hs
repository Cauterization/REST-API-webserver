{-# LANGUAGE ImportQualifiedPost #-}

module App.Opts where

import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    short,
    strOption,
    switch,
    (<**>),
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
