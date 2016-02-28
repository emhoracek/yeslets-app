{-# LANGUAGE OverloadedStrings #-}

module Signals.Signal where

import Data.Text (Text)
import Persons.Person

data Signal = Signal { sId        :: Int
                     , sPerson    :: Person
                     , sAction    :: Text
                     , sTopic     :: Text
                     , sYesletses :: [ Yeslets ] }
              deriving (Eq, Show)

data Yeslets = Yeslets Person deriving (Eq)
instance Show Yeslets where
  show (Yeslets p) = show $ pName p
