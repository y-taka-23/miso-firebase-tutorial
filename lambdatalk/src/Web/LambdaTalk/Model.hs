module Web.LambdaTalk.Model where

data Action =
    NoOp

data Model = Model {
    } deriving (Eq, Show)

initialAction :: Action
initialAction = NoOp

initialModel :: Model
initialModel = Model {}
