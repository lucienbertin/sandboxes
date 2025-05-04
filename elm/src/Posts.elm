module Posts exposing (main, Model, Msg)

import Browser
import Html exposing (Html, text, ul, li, div, h1)
import Http
import Json.Decode exposing (Decoder, map3, field, int, string, list)
import Html exposing (b)

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
type alias Posts = List Post

type Model
  = Failure
  | Loading
  | Success Posts

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
    { url = "http://rust.sandboxes.local/api/posts"
    , method = "GET"
    , headers = headers
    , body = Http.emptyBody
    , expect = Http.expectJson GotPosts postsDecoder
    , timeout = Nothing
    , tracker = Nothing
    }


postsDecoder : Decoder Posts
postsDecoder = list postDecoder

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
  = GotPosts (Result Http.Error Posts)

update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
  case msg of
    GotPosts result ->
      case result of
        Ok posts ->
          (Success posts, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Posts" ]
    , viewPosts model
    ]


viewPosts : Model -> Html Msg
viewPosts model =
  case model of
    Failure -> text "I could not load a post for some rease. "
    Loading -> text "Loading..."
    Success posts -> ul [] (posts |> List.map viewPost)

viewPost : Post -> Html Msg
viewPost post = li [] 
  [ b [] [text post.title]
  , text " "
  , text post.body
  ]