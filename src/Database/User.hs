module Database.User where

import App.Types (Token)
import Database.Get (Gettable, getEntitiesWith)
import Database.HasDatabase (HasDatabase (FromRowOf, ToRowOf))
import Database.Internal (getSingle)
import Entity.Internal (Entity)
import Entity.User (User)
import Extended.Text (Text)
import HKD.HKD (Display)

getUserByToken ::
  forall m a.
  ( HasDatabase m,
    Gettable (Entity User) a,
    FromRowOf m (Entity User a),
    ToRowOf m [Token],
    Eq (Entity User a)
  ) =>
  Token ->
  m (Entity User a)
getUserByToken token =
  getSingle
    =<< getEntitiesWith @(Entity User) @a
      [token]
      (<> " WHERE token = ?")

getUserByLogin ::
  forall m.
  ( HasDatabase m,
    Gettable (Entity User) Display,
    FromRowOf m (Entity User Display),
    ToRowOf m [Token]
  ) =>
  Text ->
  m (Entity User Display)
getUserByLogin login =
  getSingle
    =<< getEntitiesWith @(Entity User) @Display
      [login]
      (<> " WHERE login = ?")
