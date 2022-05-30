{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Api.User where

import Api.Get (Gettable)
import Api.Post (Postable)
import Api.Put (Puttable)
import App.AppT (Application)
import App.Error (wrongPasswordError)
import App.Getters (decodedBody, getToken)
import App.Impure (Impure (..))
import App.Result (Endpoint, text)
import App.ResultJSON (json)
import App.Types (ID (ID), Token)
import Control.Lens ((?~))
import Control.Monad.Writer (when)
import Crypto.Hash qualified as Crypto
import Data.Coerce (coerce)
import Database.HasDatabase qualified as Database
import Database.Post qualified as Database
import Database.Put qualified as Database
import Database.User qualified as Database
import Entity.Internal (Entity (..))
import Entity.User (Auth, User (..))
import Extended.Text (Text)
import Extended.Text qualified as T
import HKD.HKD
  ( Create,
    Display,
    EmptyData (emptyData),
    Front,
    Update,
  )
import Logger ((.<))
import Logger qualified

getCurrentUser ::
  forall a m.
  ( Application m,
    Gettable m (Entity User) a,
    Database.ToRowOf m [Token]
  ) =>
  m (Entity User a)
getCurrentUser = do
  Logger.info "Attempt to get user"
  token <- getToken
  Logger.debug $ "With token: " .< token
  u <- Database.getUserByToken @m @a token
  Logger.info $ "User was found : " .< u
  pure u

getMe ::
  forall m.
  ( Application m,
    Gettable m (Entity User) (Front Display),
    Database.ToRowOf m [Token]
  ) =>
  Endpoint m
getMe = const $ getCurrentUser @(Front Display) >>= json

postUser ::
  forall m.
  ( Application m,
    Postable m User,
    Impure m
  ) =>
  Endpoint m
postUser _ = do
  userToken <- genToken
  now <- getCurrentDate
  User {..} <- decodedBody @(User (Front Create))
  let hashedPass = mkHash password
  Logger.info "Attempt to post user"
  let user = User {registered = now, token = userToken, password = hashedPass, ..}
  userID <- Database.postEntity user
  json (userID, userToken)

authUser ::
  forall m.
  ( Application m,
    Impure m,
    Gettable m (Entity User) Display,
    Puttable m User Update,
    Database.ToRowOf m [Text]
  ) =>
  Endpoint m
authUser _ = do
  Logger.info "Attempt to login user"
  User {login = loginU, password = passwordU} <- decodedBody @(User Auth)
  Entity {..} <- Database.getUserByLogin loginU
  let passwordDB = password entity
  when (mkHash passwordU /= passwordDB) $ wrongPasswordError loginU
  newToken <- genToken
  Database.putEntity @User @m @Update $
    Entity
      (coerce entityID)
      $ #token ?~ newToken $ emptyData
  text newToken

mkHash :: Text -> Text
mkHash = T.show @(Crypto.Digest Crypto.Blake2b_160) . Crypto.hash . T.encodeUtf8
