{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Context where

import           Web.Fn
import           Network.Wai.Session (Session)
import qualified Database.PostgreSQL.Simple as PG
import           Data.Text (Text)
import qualified Data.Vault.Lazy as Vault
import           Data.Pool (Pool)
import           Control.Lens (makeLenses)

data Ctxt = Ctxt { _req    :: FnRequest
                 , _db     :: Pool PG.Connection
                 , _sess   :: Vault.Key (Session IO Text Text) }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req
