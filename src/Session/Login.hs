module Session.Login where

import Data.Text (Text)

data Login = Login { email    :: Text,
                     password :: Text }

type LoginId = Maybe Int
