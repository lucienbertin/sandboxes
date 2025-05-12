module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url

-- MAIN
main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL
type alias Model =
  { key : Nav.Key
  , url : Url.Url
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
  ( Model key url, Cmd.none )

-- UPDATE
type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
  LinkClicked (Browser.Internal url) ->
    ( model, Nav.pushUrl model.key (Url.toString url) )

  LinkClicked (Browser.External href) ->
    ( model, Nav.load href )

  UrlChanged url ->
    ( { model | url = url }
    , Nav.load <| Url.toString url
    )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW
view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body =
      [ text "The current URL is: "
      , b [] [ text (Url.toString model.url) ]
      , aside [] 
          [ nav []
              [ viewLink "/" "Home"
              , text " | "
              , viewLink "/posts" "Posts"
              , text " | "
              , viewLink "/health" "Health check"
              , text " | "
              , viewLink "https://www.linkedin.com/in/lucien-bertin-a0189387" "LinkedIn"
              ]
          ]
      ]
  }

viewLink : String -> String -> Html msg
viewLink path label =
  a [ href path ] [ text label ]