module Posts exposing (main, Model, Msg)

import Browser
import Html exposing (Html, text, ul, li, h1, main_, b)
import Http
import Json.Decode exposing (Decoder, map4, map2, field, int, string, list)


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL
type alias Author =
  { first_name : String
  , last_name : String
  }
type alias Post = 
  { id     : Int
  , title  : String
  , body   : String
  , author : Author
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


authorDecoder : Decoder Author
authorDecoder = map2 Author
  (field "first_name" string)
  (field "last_name" string)

postDecoder : Decoder Post
postDecoder = map4 Post
  (field "id" int)
  (field "title" string)
  (field "body" string)
  (field "author" authorDecoder)

postsDecoder : Decoder Posts
postsDecoder = list postDecoder

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

type Msg = GotPosts (Result Http.Error Posts)

update : Msg -> Model -> (Model, Cmd Msg)
update msg _ = case msg of
  GotPosts (Ok posts) -> (Success posts, Cmd.none)
  GotPosts (Err _)    -> (Failure, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
  main_ []
    [ h1 [] [ text "Posts" ]
    , viewPosts model
    ]


viewPosts : Model -> Html Msg
viewPosts model = case model of
  Failure -> text "I could not load a post for some rease. "
  Loading -> text "Loading..."
  Success posts -> ul []  <| List.map viewPost posts

viewPost : Post -> Html Msg
viewPost post = li [] 
  [ b [] [text post.title]
  , text " by "
  , viewAuthor <| post.author
  ]
viewAuthor : Author -> Html Msg
viewAuthor author = text <| author.first_name ++ " " ++ author.last_name