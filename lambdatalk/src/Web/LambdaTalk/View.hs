{-# LANGUAGE OverloadedStrings #-}

module Web.LambdaTalk.View (view) where

import Miso
    ( Attribute
    , View
    , button_
    , classList_
    , class_
    , div_
    , figure_
    , h1_
    , header_
    , href_
    , img_
    , input_
    , onClick
    , onInput
    , placeholder_
    , section_
    , src_
    , text
    , type_
    , value_
    )
import Miso.String ( MisoString, fromMisoString, ms )

import Web.LambdaTalk.Model ( Action(..), Model(..), User(..) )

view :: Model -> View Action
view model = div_ [] [
      navView model
    , inputView model
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
        , button_
            [ class_ "btn"
            , href_ github
            ] [ text "Source" ]
        ]
    ]

signInButton :: Maybe User -> View Action
signInButton mUser = case mUser of
    Nothing ->
        button_
            [ classes_ [ "btn", "btn-primary" ]
            , onClick SignIn
            ] [ text "Sign In" ]
    Just _ ->
        button_
            [ classes_ [ "btn", "btn-primary" ]
            , onClick SignOut
            ] [ text "Sign Out" ]

inputView :: Model -> View Action
inputView model = case currentUser model of
    Nothing -> div_ [] []
    Just user ->
        div_ [] [
              div_ [ class_ "tile" ] [
                  div_ [ class_ "tile-icon" ] [
                      figure_
                        [ class_ "avatar" ]
                        [ img_ [ src_ . ms $ photoURL user ] ]
                    ]
                , div_ [ class_ "tile-content" ] [
                      input_
                        [ class_ "form-input"
                        , type_ "text"
                        , placeholder_ "What's happening?"
                        , onInput (SetInput . fromMisoString)
                        , value_ . ms $ currentInput model
                        ]
                    ]
                , div_ [ class_ "tile-action" ] [
                      button_
                        [ classes_ [ "btn", "btn-primary" ]
                        ] [ text . ms $ currentInput model ]
                    ]
                ]
            , div_ [ class_ "divider" ] []
            ]

github :: MisoString
github = "https://github.com/y-taka-23/miso-firebase-tutorial"

classes_ :: [MisoString] -> Attribute action
classes_ cs = classList_ $ zipWith (,) cs $ repeat True
