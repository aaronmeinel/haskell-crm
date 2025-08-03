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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ConstituentEntity
    name String
    email String
    deriving Show Eq
|]

toDomain :: ConstituentEntity -> Constituent
toDomain (ConstituentEntity name email) = Constituent name email

fromDomain :: Constituent -> ConstituentEntity
fromDomain (Constituent name email) = ConstituentEntity name email


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
