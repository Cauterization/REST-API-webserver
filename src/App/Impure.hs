module App.Impure where

import App.Types (Date, Token)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor ((<&>))
import Data.Time qualified as Time
import Extended.Text qualified as T
import System.Random (randomRIO)

class Impure m where
  getCurrentDate :: m Date
  default getCurrentDate :: MonadIO m => m Date
  getCurrentDate = liftIO $ Time.getCurrentTime <&> Time.utctDay
  genToken :: m Token
  default genToken :: MonadIO m => m Token
  genToken =
    let chars = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z']
     in fmap T.pack $
          replicateM 16 $ do
            idx <- randomRIO (0, length chars - 1)
            return $ chars !! idx
