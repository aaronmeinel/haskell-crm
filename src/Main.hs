
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import App.Env (getDbConnectInfo)
import App.ConstituentRepoPostgres ()
import App.ListConstituents (listConstituents)
import App.AddConstituent (addConstituentAndList)
import Domain.Constituent (Constituent(..))
import qualified Domain.ConstituentRepo as Repo

import Servant
import Servant.HTML.Lucid
import Network.Wai.Handler.Warp (run)
import Lucid
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import qualified Configuration.Dotenv as Dotenv


import Database.Persist.Postgresql (withPostgresqlConn, runMigration, runSqlConn)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import App.ConstituentRepoPostgres (migrateAll)



-- API definition
type CRMApi = "constituents" :> Get '[HTML] (Html ())
           :<|> "add" :> QueryParam' '[Required] "name" String
                        :> QueryParam' '[Required] "email" String
                        :> Post '[HTML] (Html ())

crmServer :: Server CRMApi
crmServer = listHandler :<|> addHandler
  where
    listHandler = do
      cs <- liftIO Repo.listConstituents
      pure $ renderHtml cs
    addHandler name email = do
      _ <- liftIO $ Repo.addConstituent (Constituent name email)
      cs <- liftIO Repo.listConstituents
      pure $ renderHtml cs

renderHtml :: [Constituent] -> Html ()
renderHtml cs = html_ $ body_ $ do
  h1_ "Constituents"
  ul_ $ mapM_ (\c -> li_ (toHtml (name c <> " (" <> email c <> ")"))) cs

crmApi :: Proxy CRMApi
crmApi = Proxy

main :: IO ()

main = do
  Dotenv.loadFile Dotenv.defaultConfig
  connStr <- getDbConnectInfo
  putStrLn $ "Postgres connection string: " ++ connStr
  -- Run migrations at startup
  runResourceT $ runNoLoggingT $ withPostgresqlConn (fromString connStr) $ \backend ->
    runSqlConn (runMigration migrateAll) backend
  putStrLn "Running on http://localhost:8080"
  run 8080 (serve crmApi crmServer)

