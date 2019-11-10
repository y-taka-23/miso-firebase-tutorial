{-# LANGUAGE DeriveGeneric #-}

module Web.LambdaTalk.Model
    ( Action(..)
    , Message(..)
    , User(..)
    , initialAction
    , initialModel
    ) where

import Data.Text                   ( Text )
import GHC.Generics                ( Generic )
import Language.Javascript.JSaddle ( FromJSVal, ToJSVal )

data Action =
      NoOp
    | SignIn
    | SignOut
    | SetUser (Maybe User)
    | SetInput Text
    | ResetInput
    | SaveMessage
    | LoadMessage Message

data Model = Model {
      currentUser  :: Maybe User
    , currentInput :: Text
    , messages     :: [Message]
    } deriving (Eq, Show)

data User = User {
      displayName :: Text
    , photoURL    :: Text
    } deriving (Eq, Show, Generic)

instance FromJSVal User
instance ToJSVal   User

data Message = Message {
      author  :: User
    , content :: Text
    } deriving (Eq, Show, Generic)

instance FromJSVal Message
instance ToJSVal   Message

initialAction :: Action
initialAction = NoOp

initialModel :: Model
initialModel = Model {
      currentUser  = Nothing
    , currentInput = mempty
    , messages     = []
    }
