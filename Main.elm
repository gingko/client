port module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import String
import Tuple exposing (first, second)
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


main : Program Json.Value Model Msg
main =
  programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


port activateCards : (Int, List (List String)) -> Cmd msg
port attemptUpdate : String -> Cmd msg
port message : (String, Json.Encode.Value) -> Cmd msg


-- MODEL


type alias Model =
  { data : Trees.Model
  , viewState : ViewState
  , nextId : Int
  , saved : Bool
  }


defaultModel : Model
defaultModel =
  { data = Trees.defaultModel
  , viewState = 
      { active = "0"
      , activePast = []
      , activeFuture = []
      , descendants = []
      , editing = Just "0"
      }
  , nextId = 1
  , saved = True
  }


init : Json.Value -> (Model, Cmd Msg)
init savedState =
  let
    activateCmd mdl =
      let
        activeTree =
          getTree mdl.viewState.active mdl.data.tree ? defaultTree

        desc =
          activeTree
            |> getDescendants
            |> List.map .id

        anc = 
          getAncestors mdl.data.tree activeTree []
            |> List.map .id

        flatCols =
          mdl.data.columns
            |> List.map (\c -> List.map (\g -> List.map .id g) c)
            |> List.map List.concat

        allIds =
          anc
          ++ [mdl.viewState.active]
          ++ desc
      in
      activateCards 
        ( getDepth 0 mdl.data.tree mdl.viewState.active
        , centerlineIds 
          flatCols
          allIds
          mdl.viewState.activePast
        )

  in
  case Json.decodeValue modelDecoder savedState of
    Ok model ->
      let
        newModel =
          { model 
            | data = Trees.updateColumns model.data
          }
      in
      newModel
        ! [ activateCmd newModel
          , focus model.viewState.active
          ]
    Err err ->
      let
        deb = Debug.log "init decode error" err
      in
      defaultModel 
        ! [ activateCmd defaultModel
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
      let
        vs = model.viewState
        activeTree_ = getTree id model.data.tree
        newPast =
          case (id == vs.active) of
            True ->
              vs.activePast
            False ->
              vs.active :: vs.activePast |> List.take 40
      in
      case activeTree_ of
        Just activeTree ->
          let
            desc =
              activeTree
                |> getDescendants
                |> List.map .id

            anc = 
              getAncestors model.data.tree activeTree []
                |> List.map .id

            flatCols =
              model.data.columns
                |> List.map (\c -> List.map (\g -> List.map .id g) c)
                |> List.map List.concat

            allIds =
              anc
              ++ [id]
              ++ desc

            newModel =
              { model
                | viewState = 
                  { vs
                    | active = id
                    , activePast = newPast
                    , activeFuture = []
                    , descendants = desc
                  }
              }
          in
          newModel
            ! [ activateCards 
                  ( getDepth 0 model.data.tree id
                  , centerlineIds flatCols allIds newPast
                  )
              ]

        Nothing ->
          model ! []

    GoLeft id ->
      let
        targetId =
          getParent id model.data.tree ? defaultTree |> .id
      in
      update (Activate targetId) model

    GoDown id ->
      let
        targetId =
          case getNextInColumn id model.data.tree of
            Nothing -> id
            Just ntree -> ntree.id
      in
      update (Activate targetId) model

    GoUp id ->
      let
        targetId =
          case getPrevInColumn id model.data.tree of
            Nothing -> id
            Just ptree -> ptree.id
      in
      update (Activate targetId) model

    GoRight id ->
      let
        tree =
          getTree id model.data.tree

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
        | viewState = { vs | active = id, editing = Just id }
      } 
        ! [ focus id ]

    AttemptUpdateCard id ->
      let
        tree_ =
          getTree id model.data.tree
      in
      case tree_ of
        Just tree ->
          model ! [attemptUpdate id]

        Nothing ->
          model ! []
            |> andThen (UpdateCardError "Elm error: Card not found in tree.")

    UpdateCard (id, str) ->
      { model
        | data = Trees.update (Trees.Upd id str) model.data
        , viewState = { vs | active = id, editing = Nothing }
      }
        ! [] 
        |> andThen SaveTemp

    UpdateCardError err ->
      Debug.crash err
    
    DeleteCard id ->
      let
        filteredActive =
          vs.activePast
            |> List.filter (\a -> a /= id)

        parent_ = getParent id model.data.tree
        prev_ = getPrevInColumn id model.data.tree
        next_ = getNextInColumn id model.data.tree

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
        | data = Trees.update (Trees.Del id) model.data
        , viewState = { vs | activePast = filteredActive }
      }
        ! []
        |> andThen (Activate nextToActivate)
        |> andThen SaveTemp

    CancelCard ->
      { model 
        | viewState = { vs | editing = Nothing }
      } 
        ! []


    -- === Card Insertion  ===

    Insert subtree pid idx ->
      let
        newId = subtree.id
      in
      { model
        | data = Trees.update (Trees.Ins subtree pid idx) model.data
        , nextId = model.nextId + 1
      }
        ! []
        |> andThen (OpenCard newId subtree.content)
        |> andThen (Activate newId)
        |> andThen SaveTemp

    InsertAbove id ->
      let
        idx =
          getIndex id model.data.tree ? 999999

        pid_ =
          getParent id model.data.tree |> Maybe.map .id

        insertMsg =
          case pid_ of
            Nothing ->
              NoOp

            Just pid ->
              Insert (blankTree model.nextId) pid idx
      in
      case vs.editing of
        Nothing ->
          update insertMsg model

        Just id ->
          update (AttemptUpdateCard id) model
            |> andThen insertMsg -- TODO: Only if SaveSuccess

    InsertBelow id ->
      let
        idx =
          getIndex id model.data.tree ? 999999

        pid_ =
          getParent id model.data.tree |> Maybe.map .id

        insertMsg =
          case pid_ of
            Nothing ->
              NoOp

            Just pid ->
              Insert (blankTree model.nextId) pid (idx+1)
      in
      case vs.editing of
        Nothing ->
          update insertMsg model

        Just id ->
          update (AttemptUpdateCard id) model
            |> andThen insertMsg -- TODO: Only if SaveSuccess

    InsertChild pid ->
      let
        insertMsg =
          Insert (blankTree model.nextId) pid 999999
      in
      case vs.editing of
        Nothing ->
          update insertMsg model

        Just id ->
          update (AttemptUpdateCard id) model
            |> andThen insertMsg -- TODO: Only if SaveSuccess

    -- === Card Moving  ===

    Move subtree pid idx ->
      let
        newData = Trees.update (Trees.Mov subtree pid idx) model.data
      in
      if newData == model.data then
        model ! []
      else
        { model
          | data = newData
          , saved = False
        }
          ! []
          |> andThen (Activate subtree.id)
          |> andThen SaveTemp

    MoveUp id ->
      let
        tree_ =
          getTree id model.data.tree

        pid_ =
          getParent id model.data.tree
            |> Maybe.map .id

        refIdx_ =
          getIndex id model.data.tree
      in
      case (tree_, pid_, refIdx_) of
        (Just tree, Just pid, Just refIdx) ->
          update (Move tree pid (refIdx-1)) model
        _ ->
          model ! []

    MoveDown id ->
      let
        tree_ =
          getTree id model.data.tree

        pid_ =
          getParent id model.data.tree
            |> Maybe.map .id

        refIdx_ =
          getIndex id model.data.tree
      in
      case (tree_, pid_, refIdx_) of
        (Just tree, Just pid, Just refIdx) ->
          update (Move tree pid (refIdx+1)) model
        _ ->
          model ! []

    MoveLeft id ->
      let
        tree_ =
          getTree id model.data.tree

        parentId =
          getParent id model.data.tree
            |> Maybe.map .id
            |> Maybe.withDefault "invalid"

        parentIdx_ =
          getIndex parentId model.data.tree

        grandparentId_ =
          getParent parentId model.data.tree
            |> Maybe.map .id

      in
      case (tree_, grandparentId_, parentIdx_) of
        (Just tree, Just gpId, Just refIdx) ->
          update (Move tree gpId (refIdx+1)) model
        _ ->
          model ! []

    MoveRight id ->
      let
        tree_ =
          getTree id model.data.tree

        prev_ =
          getPrev id model.data.tree
            |> Maybe.map .id
      in
      case (tree_, prev_) of
        (Just tree, Just prev) ->
          update (Move tree prev 999999) model
        _ ->
          model ! []


    -- === History ===

    -- === Ports ===

    SaveTemp ->
      let
        newModel =
          { model
            | saved = False
          }
      in
      newModel ! [ message ("save-temp", modelToValue newModel) ]

    Confirm tag title msg ->
      model
        ! [ message
            ( tag
            , Json.Encode.object  [ ("title", Json.Encode.string title)
                                  , ("message", Json.Encode.string msg)
                                  ]
            )
          ]

    ExternalCommand (cmd, arg) ->
      case cmd of
        "keyboard" ->
          model ! [run (HandleKey arg)]

        "confirm-cancel" ->
          if arg == "true" then
            update CancelCard model
          else
            model ! []

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
            (\id -> AttemptUpdateCard id)

        "enter" ->
          normalMode model
            (OpenCard vs.active (getContent vs.active model.data.tree))

        "esc" ->
          update CancelCard model -- TODO

        "mod+backspace" ->
          normalMode model
            (DeleteCard vs.active)

        "mod+j" ->
          update (InsertBelow vs.active) model

        "mod+down" ->
          normalMode model
            (InsertBelow vs.active)

        "mod+k" ->
          update (InsertAbove vs.active) model

        "mod+up" ->
          normalMode model
            (InsertAbove vs.active)

        "mod+l" ->
          update (InsertChild vs.active) model

        "mod+right" ->
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

port externals : ((String, String) -> msg) -> Sub msg -- ~ Sub (String, String)
port updateSuccess : ((String, String) -> msg) -> Sub msg
port updateError : (String -> msg) -> Sub msg
port data : (Json.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ externals ExternalCommand
    , updateSuccess UpdateCard
    , updateError UpdateCardError
    , data DataIn
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
