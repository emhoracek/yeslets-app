{-# LANGUAGE OverloadedStrings #-}

module Persons.Data where

import qualified Database.PostgreSQL.Simple as PG
import           Data.Text (Text)
import           Data.Pool (Pool, withResource)
import           Data.Maybe (listToMaybe)

import Persons.Person

queryPersons :: Pool PG.Connection -> IO [Person]
queryPersons pgpool =
  withResource pgpool (\conn ->
    PG.query_ conn "SELECT id, name, email FROM persons" :: IO [ Person ])

findPersonById :: Int -> Pool PG.Connection -> IO (Maybe Person)
findPersonById id' pgpool =
  withResource pgpool
    (\conn -> do
         let q = "SELECT id, name, email FROM persons WHERE id = ?"
         ps <- PG.query conn q (PG.Only id') :: IO [Person]
         return $ listToMaybe ps)

findPersonByEmail :: Text -> Pool PG.Connection -> IO (Maybe Person)
findPersonByEmail email' pgpool =
  withResource pgpool
    (\conn -> do
         let q = "SELECT id, name, email FROM persons WHERE email = ?"
         ps <- PG.query conn q (PG.Only email') :: IO [Person]
         return $ listToMaybe ps)

findPersonsByName :: Text -> Pool PG.Connection -> IO [Person]
findPersonsByName name' pgpool =
  withResource pgpool
    (\conn ->
         let q = "SELECT id, name, email FROM persons WHERE name = ?" in
         PG.query conn q (PG.Only name') :: IO [Person] )
