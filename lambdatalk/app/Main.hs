module Main (main) where

import Miso ( App(..), defaultEvents, startApp )

import qualified Web.LambdaTalk.Model        as Model
import qualified Web.LambdaTalk.Subscription as Subscription
import qualified Web.LambdaTalk.Update       as Update
import qualified Web.LambdaTalk.View         as View

main :: IO ()
main = startApp App {
      initialAction = Model.initialAction
    , model         = Model.initialModel
    , update        = Update.update
    , view          = View.view
    , subs          = Subscription.subs
    , events        = defaultEvents
    , mountPoint    = Nothing
    }
