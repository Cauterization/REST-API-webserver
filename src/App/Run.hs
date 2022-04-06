module App.Run where

import Control.Monad.Catch 
import Control.Monad.Reader 

import Data.Kind (Type)

import App.Internal
import App.Result
import App.Config qualified as App
import App.QueryParams

import App.Types

import Logger qualified 
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Lazy as BL
import Extended.Text qualified as T

data ToResponse = ToResponse 
    { respStatus  :: HTTP.Status
    , respHeaders :: [HTTP.Header] 
    , respBody    :: Body
    } deriving Show

runApp :: (Monad m, MonadCatch (AppT m)) => Env m -> AppT m AppResult -> m ToResponse
runApp env app = either responseFromError responseFromResult <$>
    runReaderT (unApp $ try app) env

responseFromResult :: AppResult -> ToResponse
responseFromResult = \case
    ResText t  -> r200text $ BL.fromStrict $ T.encodeUtf8 t
    ResJSON bl -> r200json bl
    ResPicture -> error "runRouter picture result"
  where
    r200text = ToResponse HTTP.status200 
        [("ContentType","text/plain; charset=utf-8")]
    r200json = ToResponse HTTP.status200 [("ContentType","application/json")]
    r404     = ToResponse HTTP.status404 [] "Not found."
    r400     = ToResponse HTTP.status400 []
    r500     = ToResponse HTTP.internalServerError500 [] "Internal error."

responseFromError :: AppError -> ToResponse
responseFromError = responseFromResult . ResText . T.show
