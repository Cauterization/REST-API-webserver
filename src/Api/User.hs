module Api.User where

import Control.Monad.Catch
import Control.Monad.Writer
import Crypto.Hash qualified as Crypto


import HKD.HKD

import Extended.Text (Text)
import Extended.Text qualified as T
import Entity.Internal
import App.Router
import App.Internal
import App.Types
import App.Result
import Entity.User

import Database.Database qualified as Database
import Database.Database (Database)
import Logger qualified
import Logger ((.<))
import Data.Coerce


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
    let user = User { created = now, token = userToken, password = hashedPass, .. } 
    userID <- Database.postEntity user
    json (userID, userToken)

authUser :: forall m.
    ( Application m
    , Impure m
    , Gettable m (Entity User) Display
    , Puttable m User TokenUpdate
    , Database.ToRowOf (Database.Database m) [Text]
    ) => Endpoint m
authUser _ = do
    Logger.info "Attempt to login user"
    User{login = al, password = ap} <- decodedBody @(User Auth)
    Entity{..} <- Database.getSingle 
        =<< Database.getEntitiesWith @(Entity User) @Display [al] 
            (<> " WHERE login = ?")
    let passwordDB = password entity
    when (mkHash ap /= passwordDB) $ throwM WrongPassword
    newToken <- genToken
    let User{..} = emptyData @(User TokenUpdate)
    Database.putEntity @User @_ @TokenUpdate 
        [coerce entityID] User{token = Just newToken, ..}
    text newToken

mkHash :: Text -> Text
mkHash = T.show @(Crypto.Digest Crypto.Blake2b_160) . Crypto.hash . T.encodeUtf8 
