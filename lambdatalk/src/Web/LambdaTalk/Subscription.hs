module Web.LambdaTalk.Subscription (subs) where

import Miso ( Sub )

import Web.LambdaTalk.Firebase ( getCurrentUser, onAuthStateChanged )
import Web.LambdaTalk.Model    ( Action(..), User )

subs :: [Sub Action]
subs = [
      authSub SetUser
    ]

authSub :: (Maybe User -> action) -> Sub action
authSub act = \sink -> do
    onAuthStateChanged $ do
        mUser <- getCurrentUser
        sink (act mUser)
