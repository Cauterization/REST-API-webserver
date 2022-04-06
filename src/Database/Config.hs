module Database.Config where
    
import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data Config = Config
    { cConn    :: Text
    , cPagSize :: Int
    } deriving (Show, Generic, FromJSON)
