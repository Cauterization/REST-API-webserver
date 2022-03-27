module Extended.Postgres 
    ( module Database.PostgreSQL.Simple
    , module Database.PostgreSQL.Simple.ToField
    , module Database.PostgreSQL.Simple.FromField
    , module Database.PostgreSQL.Simple.FromRow
    , renderNull
    , failFromField
    ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField (FromField(..), Field, Conversion, returnError)
import Database.PostgreSQL.Simple.FromRow
 
import Data.ByteString.Builder qualified as BSB
import Data.Typeable

renderNull :: Action
renderNull = Plain (BSB.byteString "null")

failFromField :: Typeable a => Field -> a -> Conversion a
failFromField = const . flip (returnError ConversionFailed) ""