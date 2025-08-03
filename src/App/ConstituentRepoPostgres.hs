{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App.ConstituentRepoPostgres where

import Domain.Constituent (Constituent(..))
import Domain.ConstituentRepo
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

instance FromRow Constituent where
  fromRow = Constituent <$> field <*> field

instance ToRow Constituent where
  toRow (Constituent n e) = [toField n, toField e]

instance MonadConstituentRepo IO where
  addConstituent c = do
    conn <- connectPostgreSQL "dbname=yourdb user=youruser password=yourpass"
    _ <- execute conn "INSERT INTO constituents (name, email) VALUES (?, ?)" (name c, email c)
    pure ()

  listConstituents = do
    conn <- connectPostgreSQL "dbname=yourdb user=youruser password=yourpass"
    query_ conn "SELECT name, email FROM constituents"
