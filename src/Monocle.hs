{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Monocle
    ( runApp
    ) where

import Control.Monad.Trans.Except
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Data.Monoid

-- API
type MonocleApi = "monocle" :> Get '[JSON] [Environment]

monocleApi :: Proxy MonocleApi
monocleApi = Proxy

-- SERVER

runApp :: IO ()
runApp = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " <> show port))
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve monocleApi server

server :: Server MonocleApi
server = getVersions

getVersions :: Handler [Environment]
getVersions = return [mockEnv]

mockEnv :: Environment
mockEnv = Environment (Endpoint "Dev") (ClinAppVersion "2.7.0") (VeraServerVersion "1.6.0")

data Environment =
  Environment
  { name :: Endpoint
  , webVersion :: ClinAppVersion
  , serverVersion :: VeraServerVersion
  }
  deriving (Show, Generic)

instance ToJSON Environment

newtype Endpoint = Endpoint String
  deriving (Show, Generic)

instance ToJSON Endpoint

newtype VeraServerVersion = VeraServerVersion String
  deriving (Show, Generic)

instance ToJSON VeraServerVersion

newtype ClinAppVersion = ClinAppVersion String
  deriving (Show, Generic)

instance ToJSON ClinAppVersion
