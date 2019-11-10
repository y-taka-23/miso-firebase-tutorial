module Web.LambdaTalk.Firebase
    ( getCurrentUser
    , onAuthStateChanged
    , saveMessage
    , signIn
    , signOut
    ) where

import Control.Lens                ( (^.) )
import Language.Javascript.JSaddle
    ( JSCallAsFunction
    , fromJSVal
    , function
    , js
    , js0
    , js1
    , jsg
    , new
    , runJSaddle
    , val
    , valIsNull
    )

import Web.LambdaTalk.Model ( Message, User )

signIn :: IO ()
signIn = do
    google <- jsg "firebase" ^. js "auth" ^. js "GoogleAuthProvider"
    provider <- new google ()
    jsg "firebase" ^. js0 "auth" ^. js1 "signInWithPopup" provider
    pure ()

signOut :: IO ()
signOut = do
    jsg "firebase" ^. js0 "auth" ^. js0 "signOut"
    pure ()

onAuthStateChanged :: IO () -> IO ()
onAuthStateChanged io = do
    jsg "firebase" ^. js0 "auth" ^.
            js1 "onAuthStateChanged" (function $ \_ _ [_] -> io)
    pure ()

getCurrentUser :: IO (Maybe User)
getCurrentUser = do
    user <- jsg "firebase" ^. js0 "auth" ^. js "currentUser"
    signedIn <- not <$> valIsNull user
    if signedIn
        then fromJSVal user
        else pure Nothing

saveMessage :: Message -> IO ()
saveMessage msg = do
    ref <- jsg "firebase" ^. js0 "database" ^.
            js0 "ref" ^. js1 "child" (val "messages")
    ref ^. js1 "push" (val msg)
    pure ()
