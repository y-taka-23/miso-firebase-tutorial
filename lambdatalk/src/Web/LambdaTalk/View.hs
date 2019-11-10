module Web.LambdaTalk.View (view) where

import Miso        ( View, div_, text )
import Miso.String ( ms )

import Web.LambdaTalk.Model ( Action(..), Model(..) )

view :: Model -> View Action
view model = div_ [] [ text $ ms "Hello, Miso!" ]
