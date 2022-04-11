module App.Config where

import Data.Aeson 
import GHC.Generics

import Database.Config qualified as Database

import Logger qualified

type Port = Int

data Config = Config
    { cDatabase :: Database.Config
    , cLogger   :: Logger.Config
    , cPort     :: Port
    } deriving (Show, Generic, FromJSON)