port module Main exposing (..)


import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import String
import Json.Encode
import Json.Decode as Json
import Dom
import Task
import Markdown
import List.Extra as ListExtra

import Sha1 exposing (timestamp)
import Types exposing (..)
import Trees exposing (update, view, defaultTree, blankTree)
import TreeUtils exposing (..)
import Coders exposing (modelDecoder, modelToValue, messageToValue)


main : Program Json.Value
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


port saveModel : Json.Encode.Value -> Cmd msg
port activateCards : List (List String) -> Cmd msg
port export : Json.Encode.Value -> Cmd msg
port message : Json.Encode.Value -> Cmd msg


-- MODEL


type alias Model =
  { tree : Tree
  , viewState : ViewState
  , nextId : Int
  }


defaultModel : Model
defaultModel =
  { tree = defaultTree
  , viewState = 
      { active = "0"
      , activePast = []
      , activeFuture = []
      , descendants = []
      , editing = Nothing
      , field = ""
      }
  , nextId = 1
  }


init : Json.Value -> (Model, Cmd Msg)
init savedState =
  case Json.decodeValue modelDecoder savedState of
    Ok model ->
      model ! []
    Err err ->
      let
        deb = Debug.log "init decode error" err
      in
      defaultModel ! []


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    vs = model.viewState
  in
  case msg of
    NoOp ->
      model ! []

    -- === Card Activation ===

    Activate id ->
      let
        desc =
          getTree id model.tree ? defaultTree
            |> getDescendants
            |> List.map .id
      in
      { model
        | viewState = 
            { vs 
              | active = id
              , activePast = vs.active :: vs.activePast
              , descendants = desc 
            }
      }
        ! [activateCards (centerlineIds model.tree (getTree id model.tree ? defaultTree))]

    GoLeft id ->
      let
        targetId =
          getParent id model.tree ? defaultTree |> .id
      in
      update (Activate targetId) model

    GoDown id ->
      let
        targetId =
          case getNext id model.tree of
            Nothing -> id
            Just ntree -> ntree.id
      in
      update (Activate targetId) model

    GoUp id ->
      let
        targetId =
          case getPrev id model.tree of
            Nothing -> id
            Just ptree -> ptree.id
      in
      update (Activate targetId) model

    GoRight id ->
      let
        tree =
          getTree id model.tree

        childrenIds =
          getChildren (tree ? defaultTree)
            |> List.map .id

        firstChildId =
          childrenIds
            |> List.head
            |> Maybe.withDefault id

        prevActiveOfChildren =
          vs.activePast
            |> List.filter (\a -> List.member a childrenIds)
            |> List.head
            |> Maybe.withDefault firstChildId
      in
      case tree of
        Nothing ->
          model ! []
        Just t ->
          if List.length childrenIds == 0 then
            model ! []
          else
            update (Activate prevActiveOfChildren) model
      
    -- === Card Editing  ===

    OpenCard id str ->
      { model 
        | viewState = { vs | active = id, editing = Just id, field = str }
      } 
        ! [focus id]

    UpdateField str ->
      { model 
        | viewState = { vs | field = str }
      } 
        ! []

    UpdateCard id str ->
      { model
        | tree = Trees.update (Trees.Upd id str) model.tree
        , viewState = { vs | active = id, editing = Nothing, field = "" }
      }
        ! []

    DeleteCard id ->
      { model
        | tree = Trees.update (Trees.Del id) model.tree
      }
        ! []

    CancelCard ->
      { model 
        | viewState = { vs | editing = Nothing, field = "" }
      } 
        ! []

    -- === Card Insertion  ===

    Insert subtree pid idx ->
      let
        newId = subtree.id
      in
      { model
        | tree = Trees.update (Trees.Ins subtree pid idx) model.tree
        , viewState = 
            { vs | active = newId , editing = Just newId , field = subtree.content }
        , nextId = model.nextId + 1
      }
        ! [focus newId]

    InsertAbove id ->
      let
        idx =
          getIndex id model.tree ? 999999

        pid =
          getParent id model.tree ? defaultTree |> .id
      in
      update (Insert (blankTree model.nextId) pid idx) model

    InsertBelow id ->
      let
        idx =
          getIndex id model.tree ? 999999

        pid =
          getParent id model.tree ? defaultTree |> .id
      in
      update (Insert (blankTree model.nextId) pid (idx+1)) model

    InsertChild pid ->
      update (Insert (blankTree model.nextId) pid 999999) model

    -- === Card Moving  ===

    Move subtree pid idx ->
      { model
        | tree = Trees.update (Trees.Mov subtree pid idx) model.tree
      }
        ! []

    MoveUp id ->
      let
        tree_ =
          getTree id model.tree

        pid_ =
          getParent id model.tree
            |> Maybe.map .id

        refIdx_ =
          getIndex id model.tree
      in
      case (tree_, pid_, refIdx_) of
        (Just tree, Just pid, Just refIdx) ->
          update (Move tree pid (refIdx-1)) model
        _ ->
          model ! []

    MoveDown id ->
      let
        tree_ =
          getTree id model.tree

        pid_ =
          getParent id model.tree
            |> Maybe.map .id

        refIdx_ =
          getIndex id model.tree
      in
      case (tree_, pid_, refIdx_) of
        (Just tree, Just pid, Just refIdx) ->
          update (Move tree pid (refIdx+1)) model
        _ ->
          model ! []




    -- === External Inputs ===

    ExternalCommand (cmd, arg) ->
      case cmd of
        "save-and-close" ->
          model ! [ message (messageToValue ("save-and-close", modelToValue model)) ]
        "keyboard" ->
          model ! [run (HandleKey arg)]
        _ ->
          let
            db1 = Debug.log "Unknown external command" cmd
          in
          model ! []

    HandleKey str ->
      let
        vs = model.viewState
      in
      case str of
        "mod+x" ->
          model ! [ message (messageToValue ("test-msg", modelToValue model)) ]

        "mod+s" ->
          model ! [ message (messageToValue ("save", modelToValue model)) ]

        "mod+enter" ->
          editMode model
            (\uid -> UpdateCard uid vs.field)

        "enter" ->
          normalMode model
            (OpenCard vs.active (getContent vs.active model.tree))

        "esc" ->
          editMode model (\_ -> CancelCard )

        "mod+backspace" ->
          normalMode model
            (DeleteCard vs.active)

        "mod+j" ->
          normalMode model
            (InsertBelow vs.active)

        "mod+k" ->
          normalMode model
            (InsertAbove vs.active)

        "mod+l" ->
          normalMode model
            (InsertChild vs.active)

        "h" ->
          normalMode model
            (GoLeft vs.active)

        "left" ->
          normalMode model
            (GoLeft vs.active)

        "j" ->
          normalMode model
            (GoDown vs.active)

        "down" ->
          normalMode model
            (GoDown vs.active)

        "k" ->
          normalMode model
            (GoUp vs.active)
  
        "up" ->
          normalMode model
            (GoUp vs.active)
  
        "l" ->
          normalMode model
            (GoRight vs.active)

        "right" ->
          normalMode model
            (GoRight vs.active)

        "alt+up" ->
          normalMode model
            (MoveUp vs.active)

        "alt+down" ->
          normalMode model
            (MoveDown vs.active)

        "alt+left" ->
          normalMode model
            (MoveLeft vs.active)

        "alt+right" ->
          normalMode model
            (MoveRight vs.active)

        "[" ->
          normalMode model ActivatePast

        "]" ->
          normalMode model ActivateFuture

        other ->
          let
            deb = Debug.log "keyboard" other
          in
          model ! []

    _ ->
      model ! []




-- VIEW


view : Model -> Html Msg
view model =
  (lazy2 Trees.view model.viewState model.tree)




-- SUBSCRIPTIONS

port externals : ((String, String) -> msg) -> Sub msg -- ~ Sub (String, String)
port opsIn : (Json.Encode.Value -> msg) -> Sub msg -- ~ Sub Op


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ externals ExternalCommand
    , opsIn OpIn
    ]




-- HELPERS

focus : String -> Cmd Msg
focus uid =
  Task.perform (\_ -> NoOp) (\_ -> NoOp) (Dom.focus ("card-edit-" ++ uid))
      


run : Msg -> Cmd Msg
run msg =
  Task.perform (\_ -> NoOp) (\_ -> msg ) (Task.succeed msg)


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
