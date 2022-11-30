{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeApplications  #-}

module Server.Routes (
    API,
    server,
    ) where 

import Data.Int (Int64)
import Servant

import Common.Database
import Model.Auth
import Server.ServerGen

type API = AuthAPI 

server :: Server API
server = serverAuth

type AuthAPI = "api" :> "user" :> APIGen AuthUser Int64 
          :<|> "api" :> "group" :> APIGen AuthGroup Int64

serverAuth :: Server AuthAPI
serverAuth = 
  serverGen 
    (listHandlerGen connectionString :: Handler [AuthUser]) 
    (newHandlerGen connectionString :: AuthUser -> Handler Int64)
    (viewHandlerGen connectionString :: Int64 -> Handler AuthUser)
    (updateHandlerGen connectionString :: Int64 -> AuthUser -> Handler ())
    (deleteHandlerGen connectionString (Proxy @AuthUser) :: Int64 -> Handler ())

  :<|>
  serverGen 
    (listHandlerGen connectionString :: Handler [AuthGroup]) 
    (newHandlerGen connectionString :: AuthGroup -> Handler Int64)
    (viewHandlerGen connectionString :: Int64 -> Handler AuthGroup)
    (updateHandlerGen connectionString :: Int64 -> AuthGroup -> Handler ())
    (deleteHandlerGen connectionString (Proxy @AuthGroup) :: Int64 -> Handler ())
