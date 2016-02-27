{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Web.Fn
import           Network.Wai.Handler.Warp (run)
import Control.Exception (SomeException, catch)
import Data.Serialize.Text ()
import           Network.Wai (Application, Request,
                              Response, defaultRequest, vault)
import           Network.Wai.Session (Session, withSession)
import           Network.Wai.Session.ClientSession (clientsessionStore)
import Web.ClientSession (randomKey)
import Data.Default (def)
import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.FromRow
import           Data.Text (Text, pack)
import qualified Data.Text as T
--import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import qualified Data.Vault.Lazy as Vault
import           Data.Text.Lazy (toStrict)
import           Data.Pool (Pool, createPool, withResource, destroyAllResources)
import           Control.Lens (makeLenses, (^.), _1)
import           Data.Maybe (fromJust, listToMaybe, fromMaybe)
import           Lucid
import qualified Database.Redis as R
import Data.Monoid ((<>), mempty)
import Data.Int (Int64)

data Ctxt = Ctxt { _req    :: FnRequest
                 , _db     :: Pool PG.Connection
                 , _redis  :: R.Connection
                 , _sess   :: Vault.Key (Session IO Text Text) }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

initializer :: IO Ctxt
initializer = do
  pgpool <- createPool (PG.connect (PG.ConnectInfo "localhost"
                                                   5432
                                                   "libby"
                                                   "123"
                                                   "yeslets"))
                        PG.close 1 60 20
  redis <- R.connect R.defaultConnectInfo
  session <- Vault.newKey
  return (Ctxt defaultFnRequest pgpool redis session)

site :: Ctxt -> IO Response
site ctxt =
  route ctxt [ end ==> welcomeHandler
             , path "signals" ==> signalsHandler
             , path "signal" // segment // end ==> signalHandler
             , path "signal" //
                 segment //
                 path "yeslets" //
                 segment ==> addYesletsHandler
             , path "persons" ==> personsHandler
             , path "session" ==> sessionHandler
             , path "login" // end ==> loginHandler
             , path "dologin" /? param "email"
                              /? param "password" ==> doLoginHandler
             , anything ==> staticServe "static" ]
    `fallthrough` notFoundText "Page not found."

lucidHtml :: Html () -> IO (Maybe Response)
lucidHtml h = okHtml $ toStrict $ renderText h

welcomeHandler :: Ctxt -> IO (Maybe Response)
welcomeHandler ctxt = do
  signals <- querySignals (_db ctxt)
  persons <- queryPersons (_db ctxt)
  lucidHtml $ do
    siteHeader "Yeslets!"
    div_ $ do
      h1_ "Signals"
      signalsHtml signals
    div_ $ do
      h1_ "People"
      personsHtml persons

siteHeader :: Text -> Html ()
siteHeader pageTitle = do
  html_ $ do
    head_ $ do
      title_ (toHtml pageTitle)
      link_ [ rel_ "stylesheet"
            , href_ "style.css"]

signalsHandler :: Ctxt -> IO (Maybe Response)
signalsHandler ctxt = do
  signals <- querySignals (_db ctxt)
  lucidHtml (siteHeader "signals" <> signalsHtml signals)

signalHandler :: Ctxt -> Int -> IO (Maybe Response)
signalHandler ctxt sId = do
  maybeSignal <- findSignalById sId (_db ctxt)
  let html s = lucidHtml (siteHeader "signals" <> signalHtml s)
  case maybeSignal of
   Just signal -> html signal
   Nothing -> return Nothing

signalsHtml :: [Signal] -> Html ()
signalsHtml signals = foldr (<>) mempty (map signalHtml signals)

signalHtml :: Signal -> Html ()
signalHtml signal =
  let person = sPerson signal
      name = a_ [href_ ("/persons/id" <> showT (pId person))] (toHtml $ pName person)
      act = sAction signal
      topic = sTopic signal
      yeslets = yesletsesHtml $ sYesletses signal in
  div_ [class_ "signal"] $
    p_ $ name <> (toHtml $ " wants to " <> act <> " " <> topic <> ". ") <> yeslets

personsHtml :: [Person] -> Html ()
personsHtml persons = foldr (<>) mempty (map personHtml persons)

personHtml :: Person -> Html ()
personHtml person =
  div_ [class_ "person"] $
    p_ $ a_ [href_ ("/persons/id" <> showT (pId person))]
            (toHtml $ pName person)

yesletsesHtml :: Signal -> Html ()
yesletsesHtml signal =
  let ys = sYesletses signal
      yesletsUrl = "/signal/" <> showT (sId signal) <> "/yeslets" in
  div_ [class_ "yeslets"] $ do
    case ys of
     [] -> p_ ((toHtml "Be the first to say, ") <>
               (a_ [href_ yesletsUrl] "\"Yes, lets!\""))
     _  -> do
             ul_ (foldr (<>) mempty $ map yesletsHtml ys)
             p_ (a_ [href_ yesletsUrl] "\"Yes, lets!\"")

yesletsHtml :: Yeslets -> Html ()
yesletsHtml (Yeslets p) =
  li_ $ do
    a_ [href_ ("/persons/id" <> (showT $ pId p))] (toHtml $ pName p)

loginHandler :: Ctxt -> IO (Maybe Response)
loginHandler ctxt = lucidHtml $ do
  siteHeader "login"
  h1_ "Login"
  form_ [action_ "/dologin",
         method_ "post"] $ do
    label_ [for_ "email"] $ do
      "Email"
      input_ [name_ "email",
              type_ "text"]
    label_ [for_ "password"] $ do
      "Password"
      input_ [name_ "password",
              type_ "password"]
    input_ [type_ "submit"]

sessionHandler :: Ctxt -> IO (Maybe Response)
sessionHandler ctxt = do
  let Just (getSess, putSess) = Vault.lookup (ctxt ^. sess)
                                             (vault (ctxt ^. req ._1))
  current <- fromMaybe "0" <$> getSess "visits"
  let cur = case T.decimal current of
              Left _ -> error "Bad value in session"
              Right (n, _) -> n
  putSess "visit" (showT (cur + 1 :: Int))
  okText (showT cur)

app :: IO (Application, IO())
app = do
  -- don't do this???
  (_, k) <- randomKey
  let store = clientsessionStore k
  ctxt <- initializer
  return (withSession store "_session" def (ctxt ^. sess) (toWAI ctxt site)
         ,destroyAllResources (ctxt ^. db))

-- Run initialized app on port 8000
main :: IO ()
main = do
  (app', shutdown) <- app
  catch (run 8000 app')
        (\(_ :: SomeException) -> shutdown)

data Person = Person { pId    :: Int
                     , pName  :: Text
                     , pEmail :: Text }
              deriving (Eq, Show)
instance PG.FromRow Person where
  fromRow = Person <$> field <*> field <*> field

data Signal = Signal { sId        :: Int
                     , sPerson    :: Person
                     , sAction    :: Text
                     , sTopic     :: Text
                     , sYesletses :: [ Yeslets ] }
              deriving (Eq, Show)

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

data Yeslets = Yeslets Person deriving (Eq)
instance Show Yeslets where
  show (Yeslets p) = show $ pName p

data DbYeslets = DbYeslets { yId  :: Int
                           , yPId :: Int
                           , ySId :: Int }
                 deriving (Eq, Show)
instance FromRow DbYeslets where
  fromRow = DbYeslets <$> field <*> field <*> field

toYeslets :: Pool PG.Connection -> DbYeslets -> IO Yeslets
toYeslets pgpool (DbYeslets i p s) = do
  person <- findPersonById p pgpool
  return $ Yeslets (fromJust person)

querySignals :: Pool PG.Connection -> IO [Signal]
querySignals pgpool =
  withResource pgpool
    (\conn -> do
        ss <- PG.query_ conn "SELECT id, person_id, action, topic FROM signals" :: IO [ DbSignal]
        mapM (toSignal pgpool) ss )

queryPersons :: Pool PG.Connection -> IO [Person]
queryPersons pgpool =
  withResource pgpool (\conn ->
    PG.query_ conn "SELECT id, name, email FROM persons" :: IO [ Person ])

data Login = Login { email    :: Text,
                     password :: Text }

loginPerson :: Login -> Pool PG.Connection -> IO (Maybe Person)
loginPerson login pgpool =
  withResource pgpool
    (\conn -> do
         let q = "SELECT id, name, email FROM persons WHERE email = ? AND password = crypt(?, password)"
         ps <- PG.query conn q (email login, password login) :: IO [Person]
         return $ listToMaybe ps)

findSignalById :: Int -> Pool PG.Connection -> IO (Maybe Signal)
findSignalById id' pgpool =
  withResource pgpool
    (\conn -> do
         let q = "SELECT id, person_id, action, topic FROM signals WHERE id = ?"
         dbss <- PG.query conn q (PG.Only id') :: IO [DbSignal]
         ss <- mapM (toSignal pgpool) dbss
         return $ listToMaybe ss)

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

showT :: Show a => a -> Text
showT = pack . show

addYesletsHandler :: Ctxt -> Int -> Int -> IO (Maybe Response)
addYesletsHandler ctxt sId pId = do
  rowsAffected <- addYeslets sId pId (_db ctxt)
  if rowsAffected == 1
  then okText "Yay! Yeslets added!"
  else okText "Couldn't add yeslets!"

addYeslets :: Int -> Int -> Pool PG.Connection -> IO Int64
addYeslets sId pId pgpool = do
  withResource pgpool
    (\conn ->
      let q = "INSERT INTO yesletses (signal_id, person_id) VALUES (?, ?)" in
      PG.execute conn q (sId, pId) :: IO Int64)

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

doLoginHandler :: Ctxt -> Text -> Text -> IO (Maybe Response)
doLoginHandler ctxt email pass = do
  maybePerson <- loginPerson (Login email pass) (_db ctxt)
  case maybePerson of
    Just p -> route ctxt [ path "id" ==> (\ctxt -> okText (showT $ pId p))
                         , path "name" ==> (\ctxt -> okText (showT $ pName p))
                         , path "email" ==> (\ctxt -> okText (showT $ pEmail p))
                         , anything ==> (\ctxt -> okText (showT $ p)) ]
    Nothing -> okText "Ah! Ah! Ah! YOU DIDN'T SAY THE MAGIC WORD"
