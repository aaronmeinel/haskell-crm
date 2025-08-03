
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Servant (ReqBody, FormUrlEncoded, (:>), (:<|>)(..), Post, Get, Proxy(..), Server, Handler)
import qualified Data.Text as T
import Web.FormUrlEncoded (FromForm)


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

data AddForm = AddForm { name :: String, email :: String } deriving (Show, Generic)

instance FromForm AddForm


renderHtml :: [Constituent] -> Html ()
renderHtml cs = html_ $ body_ $ do
  h1_ "Constituents"
  form_ [method_ "post", action_ "/add"] $ do
    label_ [for_ "name"] "Name: "
    input_ [type_ "text", id_ "name", name_ "name"]
    label_ [for_ "email"] " Email: "
    input_ [type_ "email", id_ "email", name_ "email"]
    button_ [type_ "submit"] "Add Constituent"
  ul_ $ mapM_ (\c -> li_ (toHtml (Domain.Constituent.name c <> " (" <> Domain.Constituent.email c <> ")"))) cs


-- API definition
type CRMApi = Get '[HTML] (Html ())
           :<|> "constituents" :> Get '[HTML] (Html ())
           :<|> "add" :> ReqBody '[FormUrlEncoded] AddForm :> Post '[HTML] (Html ())

crmServer :: Server CRMApi
crmServer = rootHandler :<|> listHandler :<|> addHandler
  where
    rootHandler = listHandler
    listHandler = do
      cs <- liftIO Repo.listConstituents
      pure $ renderHtml cs
    addHandler (AddForm name email) = do
      _ <- liftIO $ Repo.addConstituent (Constituent name email)
      cs <- liftIO Repo.listConstituents
      pure $ renderHtml cs


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

