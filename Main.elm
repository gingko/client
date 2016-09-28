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
port saveCurrentCommit : String -> Cmd msg
port saveOp : Operation -> Cmd msg
port activateCard : String -> Cmd msg


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
      , editing = Nothing
      , field = ""
      }
  }


init : Maybe State -> ( Model, Cmd Msg )
init savedState =
  case (Debug.log "savedState" savedState) of
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
        , tree = Debug.log "newTree" newTree
        , commit = Debug.log "state.commit" state.commit
        , viewState = state.viewState
        }
          ! [ ]




-- UPDATE


type Msg
    = NoOp
    | SaveTree
    | CheckoutCommit String
    | HandleKey String
    | TreeMsg Tree.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    SaveTree ->
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
          , timestamp = 123456789
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
            , saveCurrentCommit newCommit.id
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
        ! [saveCurrentCommit cid]

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
            | tree = newTree
            , viewState =
                ViewState
                  model.viewState.active
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
                  , editing = Just newId
                  , field = ""
                  }
            }
              ! [focus newId, saveOp newOp]

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
              | tree = newTree
              , viewState =
                  { active = newId
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





-- VIEW


view : Model -> Html Msg
view model =
  let
    columns = getColumns([[[ model.tree ]]])
  in
    div [ id "wrapper" ]
        [ button [onClick SaveTree][text "save"]
        , fieldset [id "history"]
            ( List.map (viewCommit model.commit) model.objects.commits )
        , div [id "app" ]
            ( columns
              |> List.map (viewColumn model.viewState)
              |> List.map (App.map TreeMsg)
            )
        ]


viewCommit : String -> Commit -> Html Msg
viewCommit current commit =
  let
    handleCheck =
      case (current == commit.id) of
        True -> (\c -> NoOp)
        False -> (\c -> CheckoutCommit commit.id)
  in
  li []
    [ label []
        [ input [ type' "radio"
                , value commit.id
                , checked (current == commit.id)
                , onCheck handleCheck
                ][]
        , text commit.id
        ]
    ]



-- SUBSCRIPTIONS

port keyboard : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  keyboard HandleKey




-- HELPERS

focus uid =
  Task.perform (\_ -> NoOp) (\_ -> NoOp) (Dom.focus ("card-edit-" ++ uid)) 
