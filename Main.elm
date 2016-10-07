port module Main exposing (..)


import Array exposing (Array)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import String
import Dom
import Task
import Markdown
import List.Extra as ListExtra

import Types exposing (..)
import Tree exposing (..)
import TreeUtils exposing (..)


main : Program (Maybe State)
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


port saveContents : List Content -> Cmd msg
port saveNodes : List Node -> Cmd msg
port saveCommit : Commit -> Cmd msg
port setCurrentCommit : String -> Cmd msg
port saveOp : Operation -> Cmd msg
port activateCards : List (List String) -> Cmd msg


-- MODEL


type alias Model =
  { objects : Objects
  , tree : Tree
  , commit : String
  , viewState : ViewState
  }


defaultModel : Model
defaultModel =
  { objects = defaultObjects
  , tree = Tree.default
  , commit = defaultCommit.id
  , viewState = 
      { active = "0"
      , descendants = []
      , editing = Nothing
      , field = ""
      }
  }


init : Maybe State -> ( Model, Cmd Msg )
init savedState =
  case savedState of
    Nothing ->
      defaultModel ! [ ]
    Just state ->
      let
        nodeId =
          state.objects.commits
            |> ListExtra.find (\c -> c.id == state.commit)
            |> Maybe.withDefault defaultCommit
            |> .rootNode

        newTree =
          buildStructure nodeId state.objects
            |> applyOperations state.objects.operations
      in
        { objects = 
          { contents = state.objects.contents
          , nodes = state.objects.nodes
          , commits = state.objects.commits
          , operations = state.objects.operations
          }
        , tree = newTree
        , commit = state.commit
        , viewState = state.viewState
        }
          ! [ ]




-- UPDATE


type Msg
    = NoOp
    | CommitChanges Int
    | CheckoutCommit String
    | TreeMsg Tree.Msg
    | ExternalCommand (String, String)
    | HandleKey String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    CommitChanges ts ->
      let
        newContents =
          getContents model.tree
            |> List.filter (\c -> not (List.member c model.objects.contents))

        newNodes =
          (treeToNodes []) model.tree
            |> List.filter (\n -> not (List.member n model.objects.nodes))

        newCommit = 
          { id = "id"
          , rootNode = treeUid model.tree
          , timestamp = ts
          , authors = ["Adriano Ferrari <adriano.ferrari@gmail.com>"]
          , committer = "Adriano Ferrari <adriano.ferrari@gmail.com>"
          , parents = [model.commit]
          , message = "Default Commit message"
          }
            |> withCommitId

      in
        { model
          | objects =
              { nodes = model.objects.nodes ++ newNodes
              , contents = model.objects.contents ++ newContents
              , commits = model.objects.commits ++ [newCommit]
              , operations = Array.empty
              }
          , commit = newCommit.id
        }
          ! [ saveContents newContents
            , saveNodes newNodes
            , saveCommit newCommit
            , setCurrentCommit newCommit.id
            ]

    CheckoutCommit cid ->
      let
        nodeId =
          model.objects.commits
            |> ListExtra.find (\c -> c.id == cid)
            |> Maybe.withDefault defaultCommit
            |> .rootNode

        newTree =
          buildStructure nodeId model.objects
      in
      { model
        | commit = cid
        , tree = newTree
      }
        ! [setCurrentCommit cid]

    TreeMsg msg ->
      case msg of
        Tree.Activate uid ->
          let
            desc =
              model.tree
                |> flip getTree uid
                |> Maybe.withDefault Tree.default
                |> getDescendants
                |> List.map (\t -> t.uid)
          in
          { model
            | viewState = ViewState uid desc model.viewState.editing model.viewState.field
          }
            ! [ activateCards (centerlineIds model.tree (getTree model.tree uid |> Maybe.withDefault Tree.default) ) ]

        Tree.OpenCard uid str ->
          { model
            | viewState =
                ViewState
                  model.viewState.active
                  []
                  (Just uid)
                  str
          }
            ! [focus uid]

        Tree.CancelCard ->
          { model
            | viewState =
                ViewState
                  model.viewState.active
                  []
                  Nothing
                  ""
          }
            ! []

        Tree.UpdateField str ->
          { model
            | viewState =
                ViewState
                  model.viewState.active
                  []
                  model.viewState.editing
                  str
          }
            ! []

        Tree.UpdateCard uid str ->
          let
            oldTree = model.tree
            newOp = Operation "Update" [Just uid, Just str]
            newTree = Tree.update (Tree.UpdateCard uid str) oldTree
          in
             
          { model
            | tree = newTree
            , viewState =
                ViewState
                  model.viewState.active
                  []
                  Nothing
                  ""
          }
            ! [saveOp newOp] 

        Tree.DeleteCard uid ->
          let
            oldTree = model.tree
            newOp = Operation "Delete" [Just uid]
            newTree = Tree.update (Tree.DeleteCard uid) oldTree
          in
          { model
            | tree = newTree
          }
            ! [saveOp newOp]


        Tree.InsertChild uid ->
          let
            oldTree = model.tree
            parentId = Just uid
            prevId_ = getLastChild model.tree uid
            nextId_ = Nothing
            newId = newUid parentId prevId_ nextId_
            newOp = Operation "Insert" [parentId, prevId_, nextId_]
            newTree = Tree.update (Insert parentId prevId_ nextId_) oldTree
          in
            { model
              | tree = newTree
              , viewState =
                  { active = newId
                  , descendants = []
                  , editing = Just newId
                  , field = ""
                  }
            }
              ! [focus newId, saveOp newOp]

        Tree.InsertBelow uid ->
          let
            oldTree = model.tree
            parentId = getParentId model.tree uid
            prevId_ = Just uid
            nextId_ = getNext model.tree uid
            newId = newUid parentId prevId_ nextId_
            newOp = Operation "Insert" [parentId, prevId_, nextId_]
            newTree = Tree.update (Insert parentId prevId_ nextId_) oldTree
          in
            { model
              | tree = newTree
              , viewState =
                  { active = newId
                  , descendants = []
                  , editing = Just newId
                  , field = ""
                  }
            }
              ! [focus newId, saveOp newOp]

        _ ->
          { model
            | tree = Tree.update msg model.tree 
          } 
            ! []

    ExternalCommand (cmd, arg) ->
      case cmd of
        "commit-changes" ->
          model ! [run (CommitChanges (arg |> String.toInt |> Result.withDefault 0))]
        "checkout-commit" ->
          model ! [run (CheckoutCommit arg)]
        "keyboard" ->
          model ! [run (HandleKey arg)]
        _ ->
          model ! []
    
    HandleKey str ->
      let
        vs = model.viewState
      in
      case str of
        "mod+enter" ->
          editMode model
            (\uid ->  TreeMsg (Tree.UpdateCard uid vs.field))

        "enter" ->
          normalMode model
            (TreeMsg 
              (Tree.OpenCard vs.active 
                (getContent model.tree vs.active
                  |> Maybe.withDefault defaultContent 
                  |> .content
                )
              )
            )

        "mod+backspace" ->
          normalMode model
            (TreeMsg (Tree.DeleteCard vs.active))

        "mod+j" ->
          normalMode model
            (TreeMsg (Tree.InsertBelow vs.active))

        "mod+l" ->
          normalMode model
            (TreeMsg (Tree.InsertChild vs.active))

        "mod+s" ->
          normalMode model
            (CommitChanges 0)

        _ ->
          model ! []





-- VIEW


view : Model -> Html Msg
view model =
  let
    columns = getColumns([[[ model.tree ]]])
  in
    div [ id "wrapper" ]
        [ div [id "app" ]
            ( columns
              |> List.map (viewColumn model.viewState)
              |> List.map (App.map TreeMsg)
            )
        ]


-- SUBSCRIPTIONS

port externals : ((String, String) -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  externals ExternalCommand




-- HELPERS

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
