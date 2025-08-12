{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module App.ConstituentRepoPostgres where

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Domain.Constituent (Constituent(..))
import Domain.ConstituentRepo
import App.Env (getDbConnectInfo)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans.Resource (runResourceT)
import Data.UUID (UUID)

import qualified Data.UUID as UUID
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ConstituentEntity
    name String
    email String
    uuid String
    deriving Show Eq
|]


toDomain :: ConstituentEntity -> Constituent
toDomain (ConstituentEntity name email uuidStr) =
  case UUID.fromString uuidStr of
    Just uuid -> Constituent name email uuid
    Nothing   -> error $ "Invalid UUID string in DB: " ++ uuidStr

fromDomain :: Constituent -> ConstituentEntity
fromDomain (Constituent name email uuid) =
  ConstituentEntity name email (UUID.toString uuid)


runDb :: SqlPersistM a -> IO a
runDb action = do
  connStr <- getDbConnectInfo
  runResourceT $
    runNoLoggingT $
      withPostgresqlConn (BS.pack connStr) $ \backend ->
        runSqlConn action backend

instance MonadConstituentRepo IO where
  addConstituent c = runDb $ insert_ (fromDomain c)
  listConstituents = runDb $ do
    ents <- selectList [] []
    pure $ map (toDomain . entityVal) ents
