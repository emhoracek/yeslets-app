{-# LANGUAGE OverloadedStrings #-}

module Persons.Person where

import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.FromRow
import           Data.Text (Text, pack)
import qualified Data.Text as T

data Person = Person { pId    :: Int
                     , pName  :: Text
                     , pEmail :: Text }
              deriving (Eq, Show)
instance PG.FromRow Person where
  fromRow = Person <$> field <*> field <*> field
