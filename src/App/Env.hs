{-# LANGUAGE OverloadedStrings #-}
module App.Env where

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

getDbConnectInfo :: IO String
getDbConnectInfo = do
  user <- fromMaybe "crmuser" <$> lookupEnv "POSTGRES_USER"
  pass <- fromMaybe "crmpassword" <$> lookupEnv "POSTGRES_PASSWORD"
  db   <- fromMaybe "crm" <$> lookupEnv "POSTGRES_DB"
  host <- fromMaybe "localhost" <$> lookupEnv "POSTGRES_HOST"
  port <- fromMaybe "5432" <$> lookupEnv "POSTGRES_PORT"
  pure $ "host=" ++ host ++ " port=" ++ port ++ " user=" ++ user ++ " password=" ++ pass ++ " dbname=" ++ db
