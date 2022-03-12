{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, asks,
  runReaderT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Aeson (Value (Null), (.=), object, FromJSON, ToJSON)
import Data.Default (def)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (Text)
-- import qualified Database.Persist as DB
-- import qualified Database.Persist.Postgresql as DB
import Network.HTTP.Types.Status (created201,
  internalServerError500, notFound404)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings,
  setFdCacheDuration, setPort)
import Network.Wai.Middleware.RequestLogger (logStdout,
  logStdoutDev)
import System.Environment (lookupEnv)
import Web.Heroku (parseDatabaseUrl)
import Web.Scotty.Trans (ActionT, Options, ScottyT,
  defaultHandler, delete, get, json, jsonData, middleware,
  notFound, param, post, put, scottyOptsT, settings,
  showError, status, verbose)

-- import Hairy.Models (Task, TaskId, migrateAll)

import Configuration.Dotenv (loadFile, defaultConfig)
import GHC.Generics

import Lib
import Web.Scotty (scotty)
import Data.Maybe (fromJust, fromMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = do
  config <- getConfig
  let portNumber = case config of
        Config _ (Port portNumber) -> portNumber
  scotty portNumber $ do
    -- Listen for POST requests on the "/users" endpoint
    post "/users" $
      do
        -- parse the request body into our CreateUserRequest type
        createUserReq <- jsonData

        -- Create our new user.
        -- In order for this compile we need to use liftIO here to lift the IO from our
        -- createUser function. This is because the `post` function from scotty expects an
        -- ActionM action instead of an IO action
        newUser <- liftIO $ createUser createUserReq

        -- Return the user ID of the new user in the HTTP response
        json newUser

    -- Listen for DELETE requests on the "/users/:userId" endpoint
    -- delete "/users/:userId" $ do
    --   -- Get the value of the userId from the URL
    --   userId <- param "userId"
    --   json userId

      -- Delete the relevant user
      -- Same as with the user creation, we need to use liftIO here.
      -- liftIO $ Db.deleteUser db userId

-- Our createUser function simply deals with constructing a DbUsr value and passes it
-- to the Db.insertUser function
createUser :: CreateUserRequest -> IO User
createUser CreateUserRequest {name = uname, password = pwd} = pure $ User {userId="1", userName=uname}

getConfig :: IO Config
getConfig = do
  loadFile defaultConfig
  e <- getEnvironment
  p <- getPort
  return Config
    { environment = e
    , port = p
    }

data Config = Config
  {
    environment :: Environment
    , port :: Port
  }

getPort :: IO Port
getPort = do
  envPort <- lookupEnv "PORT"
  let port = do
        portString <- envPort
        portNumber <- readMaybe portString
        pure $ Port portNumber
  pure $ fromMaybe (Port 8080) port

getEnvironment :: IO Environment
getEnvironment = do
  m <- lookupEnv "SCOTTY_ENV"
  pure $ maybe Development read m

newtype Port = Port { portNumber :: Int }
  deriving (Show, Eq)

data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)

data User = User
  { userId :: Text,
    userName :: Text
  }
  deriving (Generic)

-- Data type which describes the request which
-- will be received to create a user
data CreateUserRequest = CreateUserRequest
  { name :: Text,
    password :: Text
  }
  deriving (Generic)

-- We define a FromJSON instance for CreateUserRequest
-- because we will want to parse it from a HTTP request
-- body (JSON).
instance FromJSON CreateUserRequest

instance ToJSON User
