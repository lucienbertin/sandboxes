module Main exposing (main, Model, Msg)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, text, h1, main_, span)
import Html.Events exposing (onClick)

-- MAIN
main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias Model = Int

init : Model
init =
  0

-- UPDATE
type Msg
  = Increment
  | Decrement
  | Plus Int
  | Minus Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment -> model + 1
    Decrement -> model - 1
    Plus x    -> model + x
    Minus y   -> model - y

-- VIEW
view : Model -> Html Msg
view model =
  main_ []
    [ h1 [] [text "testing elm" ]
    , div []
      [ button [ onClick (Minus 17) ] [ text "-17" ]
      , button [ onClick (Minus 3) ] [ text "-3" ]
      , button [ onClick Decrement ] [ text "-" ]
      , span [] [ text (String.fromInt model) ]
      , button [ onClick Increment ] [ text "+" ]
      , button [ onClick (Plus 5) ] [ text "+5" ]
      , button [ onClick (Plus 25) ] [ text "+25" ]
      ]
    ]