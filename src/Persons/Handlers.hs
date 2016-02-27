{-# LANGUAGE OverloadedStrings #-}

module Persons.Handlers where

import           Web.Fn
import           Network.Wai (Response)
import           Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Monoid ((<>))

import Data.Monoid ((<>), mempty)
import Data.Int (Int64)
import Context
import Persons.Person
import Utils
import Persons.Data

personsHandler :: Ctxt -> IO (Maybe Response)
personsHandler ctxt =
  route ctxt [ path "id" // segment ==> personsByIdHandler
             , path "name" // segment ==> personsByNameHandler
             , path "email" // segment ==> personsByEmailHandler
             , anything ==> (\ctxt -> do
                   persons <- queryPersons (_db ctxt)
                   okText (showT persons))]

personsByIdHandler :: Ctxt -> Int -> IO (Maybe Response)
personsByIdHandler ctxt id' = do
  persons <- findPersonById id' (_db ctxt)
  okText (showT persons)

personsByNameHandler :: Ctxt -> Text -> IO (Maybe Response)
personsByNameHandler ctxt name' = do
  persons <- findPersonsByName name' (_db ctxt)
  okText (showT persons)

personsByEmailHandler :: Ctxt -> Text -> IO (Maybe Response)
personsByEmailHandler ctxt email'= do
  persons <- findPersonByEmail email' (_db ctxt)
  okText (showT persons)
