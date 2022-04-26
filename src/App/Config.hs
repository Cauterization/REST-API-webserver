module App.Config where

import Data.Aeson 
import GHC.Generics

import Database.Config qualified as Database

import Logger qualified

data Config = Config
    { cDatabase :: Database.Config
    , cLogger   :: Logger.Config
    } deriving (Show, Generic, FromJSON)