port module Main exposing (..)


import Html exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import Tuple exposing (first, second)
import Dict exposing (Dict)
import Json.Encode
import Json.Decode as Json
import Dom
import Task

import Types exposing (..)
import Trees exposing (..)
import Coders exposing (nodeDictToJson)


main : Program Json.Value Model Msg
main =
  programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


port activateCards : (Int, List (List String)) -> Cmd msg
port getText : String -> Cmd msg
port saveNodes : Json.Encode.Value -> Cmd msg




-- MODEL


type alias Model =
  { data : Trees.Model
  , pending : Dict String TreeNode
  , viewState : ViewState
  }


defaultModel : Model
defaultModel =
  { data = Trees.defaultModel
  , pending = Dict.empty
  , viewState =
      { active = "0"
      , activePast = []
      , activeFuture = []
      , descendants = []
      , editing = Just "0"
      }
  }


init : Json.Value -> (Model, Cmd Msg)
init savedState =
  defaultModel ! []




-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    vs = model.viewState
  in
  case msg of
    GetContentToSave id ->
      model ! [getText id]

    AttemptSaveContent (id, str) ->
      { model
        | pending = Trees.getChanges (Trees.Mod id str) model.data.nodes
      }
        ! []
        |> andThen AttemptSave

    AttemptSave ->
      model ! [saveNodes (model.pending |> nodeDictToJson)]

    SaveResponses res ->
      -- receive all oks or not
      model ! []


    HandleKey key ->
      case key of
        "mod+x" ->
          let _ = Debug.log "model" model in
          model ! []

        _ ->
          model ! []

    _ ->
      model ! []


andThen : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen msg (model, prevMsg) =
  let
    newStep =
      update msg model
  in
  ( first newStep, Cmd.batch [prevMsg, second newStep] )


onlyIf : Bool -> Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
onlyIf cond msg prevStep =
  if cond then
    prevStep
      |> andThen msg
  else
    prevStep




-- VIEW


view : Model -> Html Msg
view model =
  (lazy2 Trees.view model.viewState model.data)




-- SUBSCRIPTIONS


port keyboard : (String -> msg) -> Sub msg
port attemptSaveContent : ((String, String) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ keyboard HandleKey
    , attemptSaveContent AttemptSaveContent
    ]




-- HELPERS

focus : String -> Cmd Msg
focus id =
  Task.attempt (\_ -> NoOp) (Dom.focus ("card-edit-" ++ id))


run : Msg -> Cmd Msg
run msg =
  Task.attempt (\_ -> msg ) (Task.succeed msg)


editMode : Model -> (String -> Msg) -> (Model, Cmd Msg)
editMode model editing = 
  case model.viewState.editing of
    Nothing ->
      model ! []

    Just uid ->
      update (editing uid) model


normalMode : Model -> Msg -> (Model, Cmd Msg)
normalMode model msg = 
  case model.viewState.editing of
    Nothing ->
      update msg model

    Just _ ->
      model ! []
