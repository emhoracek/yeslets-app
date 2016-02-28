{-# LANGUAGE OverloadedStrings #-}

module Signals.Data where

import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.FromRow
import           Data.Pool (Pool, withResource)
import           Data.Text (Text)
import           Data.Maybe (fromJust, listToMaybe)
import           Data.Int (Int64)

import Signals.Signal
import Persons.Data

data DbSignal = DbSignal { dbSId    :: Int
                         , dbSPId   :: Int
                         , dbAction :: Text
                         , dbTopic  :: Text }
instance FromRow DbSignal where
  fromRow = DbSignal <$> field <*> field <*> field <*> field

toSignal :: Pool PG.Connection -> DbSignal -> IO Signal
toSignal pgpool (DbSignal i p a t) = do
  person <- findPersonById p pgpool
  withResource pgpool
    (\conn -> do
        let yq = "SELECT id, person_id, signal_id FROM yesletses WHERE signal_id = ?"
        ys <- PG.query conn yq (PG.Only i)
        yesletses <- mapM (toYeslets pgpool) ys
        let person' = fromJust person
        return $ Signal i person' a t yesletses)

data DbYeslets = DbYeslets { yId  :: Int
                           , yPId :: Int
                           , ySId :: Int }
                 deriving (Eq, Show)
instance FromRow DbYeslets where
  fromRow = DbYeslets <$> field <*> field <*> field

toYeslets :: Pool PG.Connection -> DbYeslets -> IO Yeslets
toYeslets pgpool (DbYeslets _ p _) = do
  person <- findPersonById p pgpool
  return $ Yeslets (fromJust person)

querySignals :: Pool PG.Connection -> IO [Signal]
querySignals pgpool =
  withResource pgpool
    (\conn -> do
        ss <- PG.query_ conn "SELECT id, person_id, action, topic FROM signals" :: IO [ DbSignal]
        mapM (toSignal pgpool) ss )

addYeslets :: Int -> Int -> Pool PG.Connection -> IO Int64
addYeslets sId' pId pgpool = do
  withResource pgpool
    (\conn ->
      let q = "INSERT INTO yesletses (signal_id, person_id) VALUES (?, ?)" in
      PG.execute conn q (sId', pId) :: IO Int64)

removeYeslets :: Int -> Int -> Pool PG.Connection -> IO Int64
removeYeslets sId' pId' pgpool = do
  withResource pgpool
    (\conn ->
      let q = "DELETE FROM yesletses WHERE signal_id = ? AND person_id = ?" in
      PG.execute conn q (sId', pId') :: IO Int64)

findSignalById :: Int -> Pool PG.Connection -> IO (Maybe Signal)
findSignalById id' pgpool =
  withResource pgpool
    (\conn -> do
         let q = "SELECT id, person_id, action, topic FROM signals WHERE id = ?"
         dbss <- PG.query conn q (PG.Only id') :: IO [DbSignal]
         ss <- mapM (toSignal pgpool) dbss
         return $ listToMaybe ss)
