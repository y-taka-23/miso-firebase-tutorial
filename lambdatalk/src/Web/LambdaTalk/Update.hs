module Web.LambdaTalk.Update (update) where

import Miso ( Effect, noEff, (<#) )

import Web.LambdaTalk.Firebase ( saveMessage, signIn, signOut )
import Web.LambdaTalk.Model    ( Action(..), Message(..), Model(..) )

update :: Action -> Model -> Effect Action Model
update NoOp model = noEff model
update SignIn model = model <# do
    signIn
    pure NoOp
update SignOut model = model <# do
    signOut
    pure NoOp
update (SetUser mUser) model = noEff model { currentUser = mUser }
update (SetInput input) model = noEff model { currentInput = input }
update ResetInput model = noEff model { currentInput = mempty }
update SaveMessage model = model <# do
    case newMessage model of
        Nothing -> pure NoOp
        Just msg -> do
            saveMessage msg
            pure ResetInput

newMessage :: Model -> Maybe Message
newMessage model = do
    user <- currentUser model
    return Message { author = user, content = currentInput model }
