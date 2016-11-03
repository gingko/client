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

import Types exposing (..)
import Trees exposing (update, view, defaultTree, blankTree)
import TreeUtils exposing (..)
import Coders exposing (modelDecoder, modelToValue)


main : Program Json.Value
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


port activateCards : List (List String) -> Cmd msg
port message : (String, Json.Encode.Value) -> Cmd msg


-- MODEL


type alias Model =
  { tree : Tree
  , treePast : List Tree
  , treeFuture : List Tree
  , viewState : ViewState
  , nextId : Int
  , saved : Bool
  }


defaultModel : Model
defaultModel =
  { tree = defaultTree
  , treePast = []
  , treeFuture = []
  , viewState = 
      { active = "0"
      , activePast = []
      , activeFuture = []
      , descendants = []
      , editing = Just "0"
      , field = ""
      }
  , nextId = 1
  , saved = True
  }


init : Json.Value -> (Model, Cmd Msg)
init savedState =
  let
    activateCmd tree id =
      activateCards (centerlineIds tree (getTree id tree ? defaultTree))
  in
  case Json.decodeValue modelDecoder savedState of
    Ok model ->
      model 
        ! [ activateCmd model.tree model.viewState.active
          , focus model.viewState.active
          ]
    Err err ->
      let
        deb = Debug.log "init decode error" err
      in
      defaultModel 
        ! [ activateCmd defaultModel.tree defaultModel.viewState.active
          , focus defaultModel.viewState.active
          ]


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
      model ! []
        |> activate id

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
      model ! []
        |> openCard id str

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
        |> addToUndo model.tree
        |> saveTemp

    DeleteCard id ->
      let
        filteredActive =
          vs.activePast
            |> List.filter (\a -> a /= id)

        parent_ = getParent id model.tree
        prev_ = getPrev id model.tree
        next_ = getNext id model.tree

        nextToActivate =
          case (parent_, prev_, next_) of
            (_, Just prev, _) ->
              prev.id

            (_, Nothing, Just next) ->
              next.id

            (Just parent, Nothing, Nothing) ->
              parent.id

            (Nothing, Nothing, Nothing) ->
              "0"
      in
      { model
        | tree = Trees.update (Trees.Del id) model.tree
        , viewState = { vs | activePast = filteredActive }
      }
        ! []
        |> activate nextToActivate
        |> addToUndo model.tree
        |> saveTemp

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
        , nextId = model.nextId + 1
      }
        ! []
        |> openCard newId subtree.content
        |> activate newId
        |> addToUndo model.tree
        |> saveTemp

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
      let
        newTree = Trees.update (Trees.Mov subtree pid idx) model.tree 
      in
      if newTree == model.tree then
        model ! []
      else
        { model
          | tree = newTree
          , saved = False
        }
          ! []
          |> addToUndo model.tree
          |> saveTemp

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


    -- === Card Moving  ===

    Undo ->
      let
        prevState_ = List.head model.treePast
      in
      case prevState_ of
        Nothing ->
          model ! []
        Just prevState ->
          { model
            | tree = prevState
            , treePast = List.drop 1 model.treePast
            , treeFuture = model.tree :: model.treeFuture
          } 
            ! []

    Redo ->
      let
        nextState_ = List.head model.treeFuture
      in
      case nextState_ of
        Nothing ->
          model ! []
        Just nextState ->
          { model
            | tree = nextState
            , treePast = model.tree :: model.treePast
            , treeFuture = List.drop 1 model.treeFuture
          } 
            ! []

    -- === External Inputs ===

    ExternalCommand (cmd, arg) ->
      case cmd of
        "keyboard" ->
          model ! [run (HandleKey arg)]
        _ ->
          let
            db1 = Debug.log "Unknown external command" cmd
          in
          model ! []

    DataIn json ->
      init json

    HandleKey str ->
      let
        vs = model.viewState
      in
      case str of
        "mod+x" ->
          let
            db1 = Debug.log "model" model
          in
          model ! []

        "mod+s" ->
          model ! [ message ("save", modelToValue model) ]

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

        "mod+z" ->
          normalMode model Undo

        "mod+r" ->
          normalMode model Redo

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


addToUndo : Tree -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToUndo oldTree (model, msg) =
  if oldTree == model.tree then
    ( model, msg )
  else
    { model
      | treePast = oldTree :: model.treePast
      , treeFuture = []
    }
      ! [msg]


activate : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
activate id (model, msg) =
  let
    vs = model.viewState
    activeTree_ = getTree id model.tree

    toPrepend =
      case (List.take 1 vs.activePast) of
        [ap] ->
          if ap == vs.active then []
          else [id]
        lst ->
          [id]
  in
  case activeTree_ of
    Just activeTree ->
      let
        desc =
          activeTree
            |> getDescendants
            |> List.map .id
      in
      { model
        | viewState = 
            { vs 
              | active = id
              , activePast = toPrepend ++ vs.activePast
              , descendants = desc 
            }
      }
        ! [ msg
          , activateCards (centerlineIds model.tree (getTree id model.tree ? defaultTree))
          ]

    Nothing ->
      ( model, msg )




openCard : String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
openCard id str (model, msg) =
  let
    vs = model.viewState
  in
  { model 
    | viewState = { vs | active = id, editing = Just id, field = str }
  } 
    ! [ msg
      , focus id
      ]




-- VIEW


view : Model -> Html Msg
view model =
  (lazy2 Trees.view model.viewState model.tree)




-- SUBSCRIPTIONS

port externals : ((String, String) -> msg) -> Sub msg -- ~ Sub (String, String)
port data : (Json.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ externals ExternalCommand
    , data DataIn
    ]




-- HELPERS

focus : String -> Cmd Msg
focus id =
  Task.perform (\_ -> NoOp) (\_ -> NoOp) (Dom.focus ("card-edit-" ++ id))


run : Msg -> Cmd Msg
run msg =
  Task.perform (\_ -> NoOp) (\_ -> msg ) (Task.succeed msg)


saveTemp : (Model, Cmd Msg) -> (Model, Cmd Msg)
saveTemp (model, cmds) =
  let
    newModel =
      { model
        | saved = False
      }
  in
    newModel ! [ message ("save-temp", modelToValue newModel), cmds ]


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
