{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Model.ModelGen (
    listGen,
    viewGen,
    newGen,
    updateGen,
    deleteGen
) where

import           Data.Int (Int64)
import           Data.Proxy (Proxy)
import           Database.Persist
import           Database.Persist.Sql

import Common.Database
import Data.Kind

listGen :: (PersistEntityBackend a ~ SqlBackend, PersistEntity a) 
        => DBInfo -> IO [a]
listGen c = runAction c (map entityVal <$> selectList [] [])

viewGen :: (ToBackendKey SqlBackend a, i ~ Int64) 
         => DBInfo -> i -> IO (Maybe a)
viewGen c eid = runAction c (get entKey)
  where
    entKey :: (ToBackendKey SqlBackend a) => Key a
    entKey = toSqlKey eid

newGen :: (ToBackendKey SqlBackend a, i ~ Int64) 
       => DBInfo -> a -> IO i
newGen c ent = fromSqlKey <$> runAction c (insert ent)

updateGen :: (ToBackendKey SqlBackend a, i ~ Int64) 
          => DBInfo -> i -> a -> IO () 
updateGen c eid ent = runAction c (replace entKey ent)
  where 
    entKey :: (ToBackendKey SqlBackend a) => Key a
    entKey = toSqlKey eid

deleteGen :: forall (a :: Type) (i :: Type). (ToBackendKey SqlBackend a, i ~ Int64)
          => DBInfo -> Proxy a -> i -> IO ()
deleteGen c _ eid = runAction c (delete @SqlBackend @a entKey)
  where
    entKey :: Key a
    entKey = toSqlKey eid
