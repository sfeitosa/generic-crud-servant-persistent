{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}

module Model.Auth (
    AuthUser,
    AuthGroup,
    migrateAuth,
) where

import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Data.Aeson hiding (Key)
import           GHC.Generics
import           Database.Persist
import           Database.Persist.Sql
import qualified Database.Persist.TH as PTH

import Common.Database

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
    AuthUser sql
        username Text
        password Text
        name Text
        email Text
      deriving Show Read Generic ToJSON FromJSON
    AuthGroup
        name Text
      deriving Show Read Generic ToJSON FromJSON
    AuthUserGroup 
        authUser AuthUserId
        authGroup AuthGroupId
      deriving Show Read Generic ToJSON FromJSON
|]

migrateAuth :: DBInfo -> IO ()
migrateAuth connString = runAction connString (runMigration migrateAll)
