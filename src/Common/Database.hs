{-# LANGUAGE OverloadedStrings #-}

module Common.Database (
    DBInfo,
    connectionString,
    runAction
)  where

import           Control.Monad.Logger
import           Control.Monad.Reader (runReaderT)
import           Database.Persist.Sqlite
import           Data.Text

type DBInfo = Text

connectionString :: DBInfo
connectionString = "db.sqlite3"

runAction :: DBInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connString action = 
  runStdoutLoggingT $ filterLogger logFilter $ withSqliteConn connString $ \backend ->
    runReaderT action backend

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False
