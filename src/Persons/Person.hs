{-# LANGUAGE OverloadedStrings #-}

module Persons.Person where

import           Database.PostgreSQL.Simple.FromRow
import           Data.Text (Text)

data Person = Person { pId    :: Int
                     , pName  :: Text
                     , pEmail :: Text }
              deriving (Eq, Show)
instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field
