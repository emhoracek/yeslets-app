{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Context where

import           Web.Fn
import           Network.Wai (Application, Request,
                              Response, defaultRequest, vault)
import           Network.Wai.Session (Session, withSession)
import           Network.Wai.Session.ClientSession (clientsessionStore)
import qualified Database.PostgreSQL.Simple as PG
import           Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Vault.Lazy as Vault
import           Data.Pool (Pool, createPool, withResource, destroyAllResources)
import           Control.Lens (makeLenses)
import qualified Database.Redis as R

data Ctxt = Ctxt { _req    :: FnRequest
                 , _db     :: Pool PG.Connection
                 , _redis  :: R.Connection
                 , _sess   :: Vault.Key (Session IO Text Text) }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req
