module Web.LambdaTalk.Update (update) where

import Miso ( Effect, noEff, (<#) )

import Web.LambdaTalk.Firebase ( signIn, signOut )
import Web.LambdaTalk.Model    ( Action(..), Model(..) )

update :: Action -> Model -> Effect Action Model
update NoOp model = noEff model
update SignIn model = model <# do
    signIn
    pure NoOp
update SignOut model = model <# do
    signOut
    pure NoOp
update (SetUser mUser) model = noEff model { currentUser = mUser }
