{-# LANGUAGE ImplicitParams #-}

module Main where
import           System.Environment
import Server.Server (runServer)
--import qualified Api.Main

main :: IO ()
main = getArgs >>= \case    
    "migrations":_ -> let ?runMigrations = True in runServer
    _              -> let ?runMigrations = True in runServer
