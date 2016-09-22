port module Main exposing (..)


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
  , operations : List Operation
  , tree : Tree
  , rootId : String
  , viewState : ViewState
  }


defaultModel : Model
defaultModel =
  { contents = [defaultContent, { defaultContent | id = "1", content = "2" }]
  , nodes = [Node "0" "0" ["1"], Node "1" "1" []]
  , operations = []
  , tree = Tree.default
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
        , operations = data.ops
        , tree = newTree
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
          , operations = []
          , rootId = newRootId
        }
          ! [saveNodes newNodes, saveContents newContents, saveRoot newRootId, saveOp (Operation "Commit" [])]

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
            newOp = Operation "Update" [Just uid, Just str]
          in
          { model
            | tree = Tree.update (Tree.UpdateCard uid str) model.tree
            , viewState =
                ViewState
                  model.viewState.active
                  Nothing
                  ""
            , operations = model.operations ++ [newOp]
          }
            ! [saveOp newOp]

        Tree.DeleteCard uid ->
          let
            newOp = Operation "Delete" [Just uid]
          in
          { model
            | tree = Tree.update (Tree.DeleteCard uid) model.tree
            , operations = model.operations ++ [newOp]
          }
            ! [saveOp newOp]


        Tree.InsertChild uid ->
          let
            parentId = Just uid
            prevId_ = getLastChild model.tree uid
            nextId_ = Nothing
            newId = newUid parentId prevId_ nextId_
            newOp = Operation "Insert" [parentId, prevId_, nextId_]
          in
            { model
              | tree = Tree.update (Insert parentId prevId_ nextId_) model.tree
              , viewState =
                  { active = newId
                  , editing = Just newId
                  , field = ""
                  }
              , operations = model.operations ++ [newOp]
            }
              ! [focus newId, saveOp newOp]

        Tree.InsertBelow uid ->
          let
            parentId = getParent model.tree uid
            prevId_ = Just uid
            nextId_ = getNext model.tree uid
            newId = newUid parentId prevId_ nextId_
            newOp = Operation "Insert" [parentId, prevId_, nextId_]
          in
            { model
              | tree = Tree.update (Insert parentId prevId_ nextId_) model.tree
              , viewState =
                  { active = newId
                  , editing = Just newId
                  , field = ""
                  }
              , operations = model.operations ++ [newOp]
            }
              ! [focus newId, saveOp newOp]

        _ ->
          { model
            | tree = Tree.update msg model.tree 
          } 
            ! []





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
