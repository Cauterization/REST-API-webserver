module Api.User where

import Control.Monad.Catch
import Control.Monad.Writer
import Crypto.Hash qualified as Crypto

import Data.Aeson (ToJSON)
import Data.Data

import HKD.HKD

import Extended.Text (Text)
import Extended.Text qualified as T
import App.Router
import App.Internal
import App.Types
import App.Result
import Entity.User

import Database.Database qualified as Database
import Database.Database (Database)
import Logger qualified
import Logger ((.<))
import qualified Data.Time as Time
import Data.Functor
import Data.Aeson.Types (FromJSON)


getMe :: forall a m.
    ( Application m
    , Gettable m User a
    , Database.ToRowOf (Database m) [Token]
    ) => IDs -> m (User a)
getMe _ = do
    Logger.info  "Attempt to get user"
    token <- getToken
    Logger.debug $ "With token: " .< token
    u <- Database.getSingle =<< Database.getEntitiesWith @User @a [token] 
        (<> " WHERE token = ? AND NOT token IS NULL")
    Logger.info $ "User was found : " .< u
    pure u

getCurrentUser ::  forall a m.
    ( Application m
    , Gettable m User (Front Display)
    , Database.ToRowOf (Database m) [Token]
    ) => Endpoint m
getCurrentUser = const $ getMe @(Front Display) [] >>= json

postUser :: forall m.
    ( Application m
    , Postable m User
    , Impure m
    ) => Endpoint m
postUser _ = do
    userToken <- genToken
    now       <- getCurrentDate
    User {..} <- decodedBody @(User (Front Create))
    let hashedPass = mkHash password
    Logger.info "Attempt to post user"
    Database.postEntity
        User { created = now, token = userToken, password = hashedPass, .. } 
    text userToken

authUser :: forall m.
    ( Application m
    , Gettable m User Display
    , Database.ToRowOf (Database.Database m) [Token]
    , Impure m
    ) => Endpoint m
authUser _ = do
    Logger.info "Attempt to login user"
    uFront <- decodedBody @(User Auth)
    uDB <- Database.getSingle =<< Database.getEntitiesWith @User @Display [login uFront] 
        (<> " WHERE login = ?")
    when (mkHash (password uFront) /= password uDB) $ throwM WrongPassword
    userToken <- genToken
    text userToken

mkHash :: Text -> Text
mkHash = T.show @(Crypto.Digest Crypto.Blake2b_160) . Crypto.hash . T.encodeUtf8 
