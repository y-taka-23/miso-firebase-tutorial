module Web.LambdaTalk.Update (update) where

import Miso ( Effect, noEff )

import Web.LambdaTalk.Model ( Action(..), Model(..) )

update :: Action -> Model -> Effect Action Model
update NoOp model = noEff model
