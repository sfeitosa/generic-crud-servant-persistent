{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
-- {-# LANGUAGE ExplicitForAll      #-}
-- {-# LANGUAGE TypeApplications    #-}

module Server.ServerGen (
    APIGen,
    serverGen,
    listHandlerGen,
    newHandlerGen,
    viewHandlerGen,
    updateHandlerGen,
    deleteHandlerGen
) where

import Data.Int (Int64)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Database.Persist
import Database.Persist.Sql
import Servant

import Common.Database
import Model.ModelGen

-- API Definition

type APIGen a i = Get '[JSON] [a]
             :<|> ReqBody '[JSON] a :> Post '[JSON] i
             :<|> Capture "id" i :> Get '[JSON] a
             :<|> Capture "id" i :> ReqBody '[JSON] a :> Put '[JSON] ()
             :<|> Capture "id" i :> Delete '[JSON] ()

serverGen :: (i ~ Int64)
          => Handler [a] -- handler for listing of 'a's
          -> (a -> Handler i) -- handler for adding an 'a'
          -> (i -> Handler a) -- handler for viewing an 'a' given its identifier of type 'i'
          -> (i -> a -> Handler ()) -- updating an 'a' with given id
          -> (i -> Handler ()) -- deleting an 'a' given its id
          -> Server (APIGen a i)
serverGen list new view upd del = 
  list :<|> new :<|> view :<|> upd :<|> del

-- Handler functions

listHandlerGen :: (PersistEntityBackend a ~ SqlBackend, PersistEntity a) 
               => DBInfo -> Handler [a]
listHandlerGen c = liftIO $ listGen c

newHandlerGen :: (ToBackendKey SqlBackend a, i ~ Int64) 
              => DBInfo -> a -> Handler i
newHandlerGen c ent = liftIO $ newGen c ent

viewHandlerGen :: (ToBackendKey SqlBackend a, i ~ Int64)
               => DBInfo -> i -> Handler a 
viewHandlerGen c eid = do 
  maybeEnt <- liftIO $ viewGen c eid 
  case maybeEnt of 
    Just ent -> return ent 
    Nothing  -> Handler (throwE $ err401 { errBody = "Could not find AuthUser with that ID" })

updateHandlerGen :: (ToBackendKey SqlBackend a, i ~ Int64)
                 => DBInfo -> i -> a -> Handler ()
updateHandlerGen c eid ent = liftIO $ updateGen c eid ent

deleteHandlerGen :: (ToBackendKey SqlBackend a, i ~ Int64) 
                 => DBInfo -> Proxy a -> i -> Handler ()
deleteHandlerGen c p eid = liftIO $ deleteGen c p eid
