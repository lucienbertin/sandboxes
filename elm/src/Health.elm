module Health exposing (main, Model, Msg)

import Browser
import Html exposing (Html, text, pre)
import Http

-- MAIN
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL
type Model
  = Failure
  | Loading
  | Success String

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "//rust.sandboxes.local/health"
      , expect = Http.expectString GotText
      }
  )


-- UPDATE
type Msg
  = GotText (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- VIEW
view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success fullText ->
      pre [] [ text fullText ]