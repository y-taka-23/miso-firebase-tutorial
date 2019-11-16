{-# LANGUAGE OverloadedStrings #-}

module Web.LambdaTalk.View (view) where

import Miso
    ( Attribute
    , View
    , a_
    , button_
    , classList_
    , class_
    , disabled_
    , div_
    , figure_
    , h1_
    , header_
    , href_
    , input_
    , placeholder_
    , section_
    , text
    , type_
    )
import Miso.String ( MisoString, ms )

import Web.LambdaTalk.Model ( Action(..), Model(..) )

view :: Model -> View Action
view model = div_ [] [
      navView model
    , div_ [ class_ "columns" ] [
        div_ [ classes_ [ "column", "col-8", "col-mx-auto" ] ] [
              inputView model
            , messageView model
            ]
        ]
    ]

navView :: Model -> View Action
navView model = header_ [ class_ "navbar" ] [
      section_ [ class_ "navbar-section" ] [
          h1_
            [ classes_ [ "navbar-brand", "text-primary" ] ]
            [ text "LambdaTalk" ]
        ]
    , section_ [ class_ "navbar-section" ] [
          signInButton
        , a_
            [ class_ "btn"
            , href_ github
            ] [ text "Source" ]
        ]
    ]

signInButton :: View Action
signInButton =
    button_
        [ classes_ [ "btn", "btn-primary" ]
        ] [ text "Sign In" ]

inputView :: Model -> View Action
inputView model =
    div_ [ class_ "tile" ] [
          div_ [ class_ "tile-icon" ] [
              figure_
                [ class_ "avatar" ]
                []
            ]
        , div_ [ class_ "tile-content" ] [
              input_
                [ class_ "form-input"
                , type_ "text"
                , placeholder_ "What's happening?"
                ]
            ]
        , div_ [ class_ "tile-action" ] [
              button_
                [ classes_ [ "btn", "btn-primary" ]
                , disabled_ True
                ] [ text "Post" ]
            ]
        ]

messageView :: Model -> View action
messageView model = div_ [] []

github :: MisoString
github = "https://github.com/y-taka-23/miso-firebase-tutorial"

classes_ :: [MisoString] -> Attribute action
classes_ cs = classList_ $ zipWith (,) cs $ repeat True
