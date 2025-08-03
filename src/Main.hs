
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
import Database.PostgreSQL.Simple (connectPostgreSQL, Connection)
import Servant
import Servant.HTML.Lucid
import Network.Wai.Handler.Warp (run)
import Lucid
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import qualified Configuration.Dotenv as Dotenv

-- API definition
type CRMApi = "constituents" :> Get '[HTML] (Html ())
           :<|> "add" :> QueryParam' '[Required] "name" String
                        :> QueryParam' '[Required] "email" String
                        :> Post '[HTML] (Html ())

crmServer :: Connection -> Server CRMApi
crmServer conn = listHandler :<|> addHandler
  where
    listHandler = do
      cs <- liftIO $ listConstituentsIO conn
      pure $ renderHtml cs
    addHandler name email = do
      _ <- liftIO $ addConstituentAndListIO conn (Constituent name email)
      cs <- liftIO $ listConstituentsIO conn
      pure $ renderHtml cs

listConstituentsIO :: Connection -> IO [Constituent]
listConstituentsIO _ = Repo.listConstituents

addConstituentAndListIO :: Connection -> Constituent -> IO [Constituent]
addConstituentAndListIO _ c = Repo.addConstituent c >> Repo.listConstituents

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
  conn <- connectPostgreSQL (fromString connStr)
  putStrLn "Running on http://localhost:8080"
  run 8080 (serve crmApi (crmServer conn))

