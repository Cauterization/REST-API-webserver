module Api.Author where



import HKD.HKD

import App.Router
import App.Internal
import App.Types
import App.Result
import Entity.Author

import Data.Coerce

import Database.Database qualified as Database
import Logger qualified

postAuthor :: forall m.
    ( Application m
    , Postable m Author
    , Impure m
    ) => Endpoint m
postAuthor _ = do
    Author{..} <- decodedBody @(Author (Front Create))
    Logger.info "Attempt to post author"
    Database.postEntity (Author (coerce user) description) >>= text
