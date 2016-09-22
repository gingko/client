port module Main exposing (..)


import Array exposing (Array)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Dom
import Task
import Markdown
import List.Extra as ListExtra

import Types exposing (..)
import Tree exposing (..)
import TreeUtils exposing (..)


main : Program (Maybe Data)
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


port saveNodes : List Node -> Cmd msg
port saveContents : List Content -> Cmd msg
port saveRoot : String -> Cmd msg
port saveOp : Operation -> Cmd msg
port activateCard : String -> Cmd msg


-- MODEL


type alias Model =
  { contents : List Content
  , nodes : List Node
  , tree : Tree
  , pastTrees : Array Tree
  , futureTrees : Array Tree
  , operations : Array Operation
  , futureOperations : Array Operation
  , rootId : String
  , viewState : ViewState
  }


defaultModel : Model
defaultModel =
  { contents = [defaultContent, { defaultContent | id = "1", content = "2" }]
  , nodes = [Node "0" "0" ["1"], Node "1" "1" []]
  , tree = Tree.default
  , pastTrees = Array.empty
  , futureTrees = Array.empty
  , operations = Array.empty
  , futureOperations = Array.empty
  , rootId = "0"
  , viewState = 
      { active = "0"
      , editing = Nothing
      , field = ""
      }
  }


init : Maybe Data -> ( Model, Cmd Msg )
init savedData =
  case savedData of
    Nothing ->
      defaultModel ! [ ]
    Just data ->
      let
        newTree =
          buildStructure data
            |> applyOperations data.ops
      in
        { contents = data.contents
        , nodes = data.nodes
        , tree = newTree
        , pastTrees = Array.empty
        , futureTrees = Array.empty
        , operations = data.ops
        , futureOperations = Array.empty
        , rootId = data.rootId
        , viewState = 
            { active = "0"
            , editing = Nothing
            , field = ""
            }
        }
          ! [ ]




-- UPDATE


type Msg
    = NoOp
    | SaveTree
    | Undo
    | Redo
    | HandleKey String
    | TreeMsg Tree.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    SaveTree ->
      let
        newNodes =
          (treeToNodes []) model.tree
            |> List.filter (\n -> not (List.member n model.nodes))
        newContents =
          getContents model.tree
            |> List.filter (\c -> not (List.member c model.contents))
        newRootId = treeUid model.tree
      in
        { model
          | nodes = model.nodes ++ newNodes
          , contents = model.contents ++ newContents
          , operations = Array.empty
          , rootId = newRootId
        }
          ! [ saveNodes newNodes
            , saveContents newContents
            , saveRoot newRootId
            , saveOp (Operation "Commit" [])
            ]

    Undo ->
      if Array.isEmpty model.pastTrees then
         model ! []
      else
        { model
          | tree =
              model.pastTrees
                |> Array.get (Array.length model.pastTrees - 1)
                |> Maybe.withDefault Tree.default
          , pastTrees = Array.slice 0 -1 model.pastTrees
          , futureTrees = Array.push model.tree model.futureTrees
        }
          ! []

    Redo ->
      if Array.isEmpty model.futureTrees then
         model ! []
      else
        { model
          | tree =
              model.futureTrees
                |> Array.get (Array.length model.futureTrees - 1)
                |> Maybe.withDefault Tree.default
          , pastTrees = Array.push model.tree model.pastTrees
          , futureTrees = Array.slice 0 -1 model.futureTrees
        }
          ! []

    HandleKey str ->
      let
        vs = model.viewState
      in
      case str of
        "mod+enter" ->
          case vs.editing of
            Nothing ->
              model ! []

            Just uid ->
              update (TreeMsg (Tree.UpdateCard uid vs.field)) model

        "enter" ->
          case vs.editing of
            Nothing ->
              update 
                (TreeMsg 
                  (Tree.OpenCard vs.active 
                    (getContent model.tree vs.active
                      |> Maybe.withDefault defaultContent 
                      |> .content
                    )
                  )
                )
                model

            Just uid ->
              model ! []

        "mod+j" ->
          case vs.editing of
            Nothing ->
              update (TreeMsg (Tree.InsertBelow vs.active)) model

            Just uid ->
              model ! []

        "mod+l" ->
          case vs.editing of
            Nothing ->
              update (TreeMsg (Tree.InsertChild vs.active)) model

            Just uid ->
              model ! []

        "mod+s" ->
          case vs.editing of
            Nothing ->
              update SaveTree model

            Just uid ->
              model ! []

        "mod+z" ->
          case vs.editing of
            Nothing ->
              update Undo model

            Just uid ->
              model ! []

        "mod+r" ->
          case vs.editing of
            Nothing ->
              update Redo model

            Just uid ->
              model ! []

        _ ->
          model ! []

    TreeMsg msg ->
      case msg of
        Tree.Activate uid ->
          { model
            | viewState = ViewState uid model.viewState.editing model.viewState.field
          }
            ! [ activateCard uid ]

        Tree.OpenCard uid str ->
          { model
            | viewState =
                ViewState
                  model.viewState.active
                  (Just uid)
                  str
          }
            ! [focus uid]

        Tree.CancelCard ->
          { model
            | viewState =
                ViewState
                  model.viewState.active
                  Nothing
                  ""
          }
            ! []

        Tree.UpdateField str ->
          { model
            | viewState =
                ViewState
                  model.viewState.active
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
            | viewState =
                ViewState
                  model.viewState.active
                  Nothing
                  ""
          }
            ! [] 
            |> stepForward oldTree newTree newOp

        Tree.DeleteCard uid ->
          let
            oldTree = model.tree
            newOp = Operation "Delete" [Just uid]
            newTree = Tree.update (Tree.DeleteCard uid) oldTree
          in
          model ! []
            |> stepForward oldTree newTree newOp


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
              | viewState =
                  { active = newId
                  , editing = Just newId
                  , field = ""
                  }
            }
              ! [focus newId]
              |> stepForward oldTree newTree newOp

        Tree.InsertBelow uid ->
          let
            oldTree = model.tree
            parentId = getParent model.tree uid
            prevId_ = Just uid
            nextId_ = getNext model.tree uid
            newId = newUid parentId prevId_ nextId_
            newOp = Operation "Insert" [parentId, prevId_, nextId_]
            newTree = Tree.update (Insert parentId prevId_ nextId_) oldTree
          in
            { model
              | viewState =
                  { active = newId
                  , editing = Just newId
                  , field = ""
                  }
            }
              ! [focus newId]
              |> stepForward oldTree newTree newOp

        _ ->
          { model
            | tree = Tree.update msg model.tree 
          } 
            ! []


stepForward : Tree -> Tree -> Operation -> (Model, Cmd Msg) -> (Model, Cmd Msg)
stepForward oldTree newTree op (model, cmds) =
  { model
    | tree = newTree
    , pastTrees = Array.push oldTree model.pastTrees
    , futureTrees = Array.empty
    , operations = Array.push op model.operations
    , futureOperations = Array.empty
  } 
    ! [cmds, saveOp op]




-- VIEW


view : Model -> Html Msg
view model =
  let
    columns = getColumns([[[ model.tree ]]])
  in
    div [ id "wrapper" ]
        [ button [onClick SaveTree][text "save"]
        , div [id "app" ]
            ( columns
              |> List.map (viewColumn model.viewState)
              |> List.map (App.map TreeMsg)
            )
        ]




-- SUBSCRIPTIONS

port keyboard : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  keyboard HandleKey




-- HELPERS

focus uid =
  Task.perform (\_ -> NoOp) (\_ -> NoOp) (Dom.focus ("card-edit-" ++ uid)) 
