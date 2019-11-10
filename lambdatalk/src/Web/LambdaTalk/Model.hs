{-# LANGUAGE DeriveGeneric #-}

module Web.LambdaTalk.Model where

import Data.Text
import GHC.Generics                ( Generic )
import Language.Javascript.JSaddle ( FromJSVal )

data Action =
      NoOp
    | SignIn
    | SignOut
    | SetUser (Maybe User)

data Model = Model {
      currentUser :: Maybe User
    } deriving (Eq, Show)

data User = User {
      displayName :: Text
    , photoURL    :: Text
    } deriving (Eq, Show, Generic)

instance FromJSVal User

initialAction :: Action
initialAction = NoOp

initialModel :: Model
initialModel = Model {
      currentUser = Nothing
    }
