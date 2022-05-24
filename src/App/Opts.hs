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
    (<**>),
  )

data Options = Options
  { optConfigPath :: FilePath,
    optShowHelpMessage :: Bool,
    optRunMigrations :: Bool
  }

configFileParser :: Parser FilePath
configFileParser =
  strOption
    ( long "conf"
    )

helpMsgParser :: Parser Bool
helpMsgParser =
  switch
    ( long "help"
        <> short 'h'
    )

migrationParser :: Parser Bool
migrationParser =
  switch
    ( long "migrate"
        <> short 'm'
    )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> configFileParser
    <*> helpMsgParser
    <*> migrationParser

runWithOpts :: IO Options
runWithOpts = do
  Options {..} <- execParser opts
  when optShowHelpMessage $ T.putStrLn helpMessage
  pure Options {..}
  where
    opts = info (optionsParser <**> helper) fullDesc

helpMessage :: Text
helpMessage =
  "This is a simple REST API web-server. Aviable commands: \
  \\n-h/--help - see this message;\
  \\n--conf FILEPATH - define configuration file location. \
  \\n-m/--migrate - run database migrations before running server"
