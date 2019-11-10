{-# LANGUAGE OverloadedStrings #-}

module Web.LambdaTalk.View (view) where

import Miso
    ( Attribute
    , View
    , a_
    , classList_
    , class_
    , div_
    , h1_
    , header_
    , href_
    , onClick
    , section_
    , text
    )
import Miso.String ( MisoString )

import Web.LambdaTalk.Model ( Action(..), Model(..), User )

view :: Model -> View Action
view model = div_ [] [
      navView model
    ]

navView :: Model -> View Action
navView model = header_ [ class_ "navbar" ] [
      section_ [ class_ "navbar-section" ] [
          h1_
            [ classes_ [ "navbar-brand", "text-primary" ] ]
            [ text "LambdaTalk" ]
        ]
    , section_ [ class_ "navbar-section" ] [
          signInButton (currentUser model)
        , a_
            [ class_ "btn"
            , href_ github
            ] [ text "Source" ]
        ]
    ]

signInButton :: Maybe User -> View Action
signInButton mUser = case mUser of
    Nothing ->
        a_
            [ classes_ [ "btn", "btn-primary" ]
            , onClick SignIn
            ] [ text "Sign In" ]
    Just _ ->
        a_
            [ classes_ [ "btn", "btn-primary" ]
            , onClick SignOut
            ] [ text "Sign Out" ]

github :: MisoString
github = "https://github.com/y-taka-23/miso-firebase-tutorial"

classes_ :: [MisoString] -> Attribute action
classes_ cs = classList_ $ zipWith (,) cs $ repeat True
