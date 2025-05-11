module Posts exposing (main, Model, Msg)

import Browser
import Html exposing (Html, text, ul, li, h1, main_, b)
import Http
import Json.Decode exposing (Decoder, map4, map2, field, int, string, list)
import Html exposing (button)
import Html.Events exposing (onClick)
import Html exposing (label)
import Html exposing (input)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)
import Html exposing (form)
import Html exposing (h2)


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

type PostsModel
  = Failure
  | Loading
  | Success Posts
type alias PostFormModel =
  { title : String 
  , body : String
  }
type alias Model =
  { posts : PostsModel
  , postForm : PostFormModel
  }

postFormInit : PostFormModel
postFormInit = PostFormModel "" ""

-- INIT
init : () -> (Model, Cmd Msg)
init _ =
  ( Model Loading postFormInit, getPosts )

-- HTTP
headers : List Http.Header
headers = 
  [ Http.header "Authorization" "Bearer eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJsdWNpZW5AYmVydC5pbiJ9.MQ2AtPRuMhZuu84jFpjbnZF3tMREpSi51YEU6yq8KBI"]
getPosts : Cmd Msg
getPosts =
  Http.request
    { url = "https://rust.sandboxes.local/api/posts"
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

type Msg
  = RefreshPosts
  | GotPosts (Result Http.Error Posts)
  | UpdateTitle String
  | UpdateBody String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  RefreshPosts        -> ({model | posts = Loading}      , getPosts)
  GotPosts (Ok posts) -> ({model | posts = Success posts}, Cmd.none)
  GotPosts (Err _)    -> ({model | posts = Failure}      , Cmd.none)
  UpdateTitle t       -> (t |> updatePostFormTitle model.postForm |> updatePostForm model , Cmd.none)
  UpdateBody b        -> (b |> updatePostFormBody  model.postForm |> updatePostForm model , Cmd.none)

updatePostFormTitle : PostFormModel -> String -> PostFormModel
updatePostFormTitle pfm t = { pfm | title = t }
updatePostFormBody : PostFormModel -> String -> PostFormModel
updatePostFormBody pfm b = { pfm | body = b }

updatePostForm : Model -> PostFormModel -> Model
updatePostForm m pf = { m | postForm = pf }



-- VIEW
view : Model -> Html Msg
view model =
  main_ []
    [ h1 [] [ text "Posts" ]
    , viewPosts model.posts
    , button [ onClick RefreshPosts ] [ text "refresh" ]
    , viewPostForm model.postForm
    ]


viewPosts : PostsModel -> Html Msg
viewPosts model = case model of
  Failure -> text "I could not load a post for some reason. "
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

viewPostForm : PostFormModel -> Html Msg
viewPostForm model = form [] 
  [ h2 [] [text "New post"]
  , viewInput "title" model.title UpdateTitle
  , viewInput "body" model.body UpdateBody
  ]
viewInput : String -> String -> (String -> msg) -> Html msg
viewInput l v toMsg = 
  label []
    [ text l
    , input [ type_ "text", value v, onInput toMsg] []
    ]