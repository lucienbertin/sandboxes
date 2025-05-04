module Posts exposing (main, Model, Msg)

import Browser
import Html exposing (Html, text, ul, li, div, h2, blockquote, p)
import Http
import Json.Decode exposing (Decoder, map3, field, int, string)
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL
type alias Post = 
    { id    : Int
    , title : String
    , body  : String
    }
-- type alias Posts = List Post

type Model
  = Failure
  | Loading
  | Success Post

-- INIT
init : () -> (Model, Cmd Msg)
init _ =
  ( Loading, getPosts )

-- HTTP
headers : List Http.Header
headers = 
    [ Http.header "Authorization" "Bearer eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJsdWNpZW5AYmVydC5pbiJ9.MQ2AtPRuMhZuu84jFpjbnZF3tMREpSi51YEU6yq8KBI"]
getPosts : Cmd Msg
getPosts =
  Http.request
    { url = "http://rust.sandboxes.local/api/post/1"
    , method = "GET"
    , headers = headers
    , body = Http.emptyBody
    , expect = Http.expectJson GotPost postDecoder
    , timeout = Nothing
    , tracker = Nothing
    }


postDecoder : Decoder Post
postDecoder =
  map3 Post
    (field "id" int)
    (field "title" string)
    (field "body" string)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

type Msg
  = GotPost (Result Http.Error Post)

update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
  case msg of
    GotPost result ->
      case result of
        Ok post ->
          (Success post, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Post" ]
    , viewPost model
    ]


viewPost : Model -> Html Msg
viewPost model =
  case model of
    Failure ->
      div []
        [ text "I could not load a post for some rease. "
        ]

    Loading ->
      text "Loading..."

    Success post ->
      div []
        [ blockquote [] [ text post.title ]
        , p [  ]
            [ text post.body ]
        ]