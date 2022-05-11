module Extended.Postgres
  ( module Database.PostgreSQL.Simple,
    module Database.PostgreSQL.Simple.ToField,
    module Database.PostgreSQL.Simple.ToRow,
    module Database.PostgreSQL.Simple.FromField,
    module Database.PostgreSQL.Simple.FromRow,
    module Database.PostgreSQL.Simple.Types,
    renderNull
  )
where

import Data.ByteString.Builder qualified as BSB
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (Conversion, Field, FromField (..), returnError)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types (PGArray (..), fromPGArray)

renderNull :: Action
renderNull = Plain (BSB.byteString "null")

