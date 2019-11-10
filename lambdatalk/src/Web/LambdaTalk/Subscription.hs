module Web.LambdaTalk.Subscription (subs) where

import Miso ( Sub )

import Web.LambdaTalk.Firebase
    ( getCurrentUser
    , onAuthStateChanged
    , onMessageAdded
    )
import Web.LambdaTalk.Model    ( Action(..), Message, User )

subs :: [Sub Action]
subs = [
      authSub SetUser
    , messageSub LoadMessage
    ]

authSub :: (Maybe User -> action) -> Sub action
authSub act = \sink -> do
    onAuthStateChanged $ do
        mUser <- getCurrentUser
        sink (act mUser)

messageSub :: (Message -> Action) -> Sub Action
messageSub act = \sink -> do
    onMessageAdded $ \mMsg -> do
        case mMsg of
            Nothing  -> sink NoOp
            Just msg -> sink (act msg)
