module Api.Author where

import Control.Monad.Writer

import Data.Aeson (ToJSON)
import Data.Data

import HKD.HKD

import Extended.Text qualified as T
import App.Router
import App.Internal
import App.Types
import App.Result
import Entity.Author

import Data.Coerce

import Database.Database qualified as Database
import Database.Database (Database)
import Logger qualified
import Logger ((.<))
import qualified Data.Time as Time
import Data.Functor
import Data.Aeson.Types (FromJSON)

postAuthor :: forall m.
    ( Application m
    , Postable m Author
    , Impure m
    ) => Endpoint m
postAuthor _ = do
    Author{..} <- decodedBody @(Author (Front Create))
    Logger.info "Attempt to post author"
    Database.postEntity (Author (coerce user) description ) >>= text

