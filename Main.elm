port module Main exposing (..)


import Array exposing (Array)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Json.Encode
import Json.Decode as Json
import Dom
import Task
import Markdown
import List.Extra as ListExtra

import Types exposing (..)
import Coders exposing (..)
import Tree exposing (..)
import TreeUtils exposing (..)


main : Program (Maybe Json.Value)
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
port saveOp : Json.Encode.Value -> Cmd msg
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
      , activePast = []
      , activeFuture = []
      , descendants = []
      , editing = Nothing
      , field = ""
      }
  }


init : Maybe Json.Value -> ( Model, Cmd Msg )
init savedState =
  case savedState of
    Nothing ->
      defaultModel ! []
    Just val ->
      let
        deb = Debug.log "val" val
      in
      defaultModel ! []

--     Nothing ->
--       defaultModel ! [ ]
--     Just state ->
--       let
--         nodeId =
--           state.objects.commits
--             |> ListExtra.find (\c -> c.id == state.commit)
--             |> Maybe.withDefault defaultCommit
--             |> .rootNode
-- 
--         newTree =
--           buildStructure nodeId state.objects
--             |> applyOperations state.objects.operations
--       in
--         { objects = 
--           { contents = state.objects.contents
--           , nodes = state.objects.nodes
--           , commits = state.objects.commits
--           , operations = state.objects.operations
--           }
--         , tree = newTree
--         , commit = state.commit
--         , viewState = state.viewState
--         }
--           ! [ ]




-- UPDATE


type Msg
    = NoOp
    | CommitChanges Int
    | CheckoutCommit String
    | InsertAbove String
    | InsertBelow String
    | InsertChild String
    | GoLeft String
    | GoDown String
    | GoUp String
    | GoRight String
    | ActivatePast
    | ActivateFuture
    | TreeMsg Tree.Msg
    | OpIn Json.Value
    | ExternalCommand (String, String)
    | HandleKey String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    vs = model.viewState
  in
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

    InsertAbove uid ->
      let
        oldTree = model.tree
        parentId_ = getParentId model.tree uid
        prevId_ = getPrev model.tree uid
        nextId_ = Just uid
        newId = newUid parentId_ prevId_ nextId_
        newOp = Ins newId parentId_ prevId_ nextId_
        newTree = Tree.update (Tree.Apply newOp) oldTree
      in
        { model
          | tree = newTree
          , viewState =
              { active = newId
              , activePast = []
              , activeFuture = []
              , descendants = []
              , editing = Just newId
              , field = ""
              }
        }
          ! [focus newId, saveOp (opToValue newOp)]


    InsertBelow uid ->
      let
        oldTree = model.tree
        parentId_ = getParentId model.tree uid
        prevId_ = Just uid
        nextId_ = getNext model.tree uid
        newId = newUid parentId_ prevId_ nextId_
        newOp = Ins newId parentId_ prevId_ nextId_
        newTree = Tree.update (Tree.Apply newOp) oldTree
      in
        { model
          | tree = newTree
          , viewState =
              { active = newId
              , activePast = []
              , activeFuture = []
              , descendants = []
              , editing = Just newId
              , field = ""
              }
        }
          ! [focus newId, saveOp (opToValue newOp)]

    InsertChild uid ->
      let
        oldTree = model.tree
        parentId_ = Just uid
        prevId_ = getLastChild model.tree uid
        nextId_ = Nothing
        newId = newUid parentId_ prevId_ nextId_
        newOp = Ins newId parentId_ prevId_ nextId_
        newTree = Tree.update (Tree.Apply newOp) oldTree
      in
        { model
          | tree = newTree
          , viewState =
              { active = newId
              , activePast = []
              , activeFuture = []
              , descendants = []
              , editing = Just newId
              , field = ""
              }
        }
          ! [focus newId, saveOp (opToValue newOp)]

    GoLeft uid ->
      let
        targetId =
          getParent model.tree uid
            |> Maybe.withDefault (blankTree uid)
            |> .uid
      in
      update (TreeMsg (Tree.Activate targetId)) model

    GoDown uid ->
      let
        targetId =
          getNext model.tree uid
            |> Maybe.withDefault uid
      in
      update (TreeMsg (Tree.Activate targetId)) model

    GoUp uid ->
      let
        targetId =
          getPrev model.tree uid
            |> Maybe.withDefault uid
      in
      update (TreeMsg (Tree.Activate targetId)) model


    GoRight uid ->
      let
        tree =
          getTree model.tree uid -- Maybe Tree
            |> Maybe.withDefault (blankTree uid) -- Tree

        childrenIds =
          tree
            |> getChildren -- List Tree
            |> List.map .uid -- List String

        firstChild = 
          getFirstChild model.tree uid |> Maybe.withDefault uid

        prevActiveOfChildren =
          vs.activePast
            |> List.filter (\a -> List.member a childrenIds) -- children in activePast
            |> List.head
            |> Maybe.withDefault firstChild
      in
      update (TreeMsg (Tree.Activate prevActiveOfChildren)) model

    ActivatePast ->
      if List.isEmpty vs.activePast then
        model ! []
      else
      let
        targetId =
          vs.activePast
            |> List.head
            |> Maybe.withDefault vs.active

        newPast =
          List.drop 1 vs.activePast

        newFuture = vs.active :: vs.activeFuture

        newViewState v =
          { v
            | active = targetId
            , activePast = newPast
            , activeFuture = newFuture
          }
      in
      { model | viewState = newViewState vs } ! []
      
        
    ActivateFuture ->
      if List.isEmpty vs.activeFuture then
        model ! []
      else
      let
        targetId =
          vs.activeFuture
            |> List.head
            |> Maybe.withDefault vs.active

        newFuture =
          List.drop 1 vs.activeFuture

        newPast = vs.active :: vs.activePast

        newViewState v =
          { v
            | active = targetId
            , activePast = newPast
            , activeFuture = newFuture
          }
      in
      { model | viewState = newViewState vs } ! []

    TreeMsg msg ->
      case msg of
        Tree.NoOp -> model ! []

        Tree.Apply op ->
          case op of
            Upd id uid str ->
              let
                oldTree = model.tree
                newOp = Upd "" uid str
                newTree = Tree.update (Tree.Apply newOp) oldTree
              in
                 
              { model
                | tree = newTree
                , viewState =
                    { active = vs.active
                    , activePast = []
                    , activeFuture = []
                    , descendants = []
                    , editing = Nothing
                    , field = ""
                    }
              }
                ! [saveOp (opToValue newOp)] 

            Del id uid ->
              let
                oldTree = model.tree
                newOp = Del "" uid
                newTree = Tree.update (Tree.Apply newOp) oldTree
              in
              { model
                | tree = newTree
              }
                ! [saveOp (opToValue newOp)]


            _ -> model ! []


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
            | viewState = 
                { active = uid
                , activePast = vs.active :: vs.activePast
                , activeFuture = vs.activeFuture
                , descendants = desc
                , editing = vs.editing 
                , field = vs.field
                }
          }
            ! [ activateCards (centerlineIds model.tree (getTree model.tree uid |> Maybe.withDefault Tree.default) ) ]

        Tree.OpenCard uid str ->
          { model
            | viewState =
                { active = vs.active
                , activePast = []
                , activeFuture = []
                , descendants = []
                , editing = (Just uid)
                , field = str
                }
          }
            ! [focus uid]

        Tree.CancelCard ->
          { model
            | viewState =
                { active = vs.active
                , activePast = []
                , activeFuture = []
                , descendants = []
                , editing = Nothing
                , field = ""
                }
          }
            ! []

        Tree.UpdateField str ->
          { model
            | viewState =
                { active = vs.active
                , activePast = []
                , activeFuture = []
                , descendants = []
                , editing = vs.editing
                , field = str
                }
          }
            ! []

    OpIn json ->
      case (Json.decodeValue opDecoder json) of
        Ok val ->
          case val of
            Ins id parentId_ prevId_ nextId_ ->
            let
              oldTree = model.tree
              newId = newUid parentId_ prevId_ nextId_
              newTree = Tree.update (Tree.Apply val) oldTree
            in
              { model
                | tree = newTree
                , viewState =
                    { active = newId
                    , activePast = []
                    , activeFuture = []
                    , descendants = []
                    , editing = Just newId
                    , field = ""
                    }
              }
                ! [focus newId]

            _ -> model ! []

        Err err ->
          let
            deb = Debug.log "err" err
          in
          model ! []


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
        "mod+x" ->
          model ! [saveOp (opToValue (Del "123" "asdf"))]

        "mod+enter" ->
          editMode model
            (\uid ->  TreeMsg (Tree.Apply (Upd "" uid vs.field)))

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

        "esc" ->
          editMode model (\_ -> TreeMsg Tree.CancelCard )

        "mod+backspace" ->
          normalMode model
            (TreeMsg (Tree.Apply (Del "" vs.active)))

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

        "j" ->
          normalMode model
            (GoDown vs.active)

        "k" ->
          normalMode model
            (GoUp vs.active)
  
        "l" ->
          normalMode model
            (GoRight vs.active)

        "[" ->
          normalMode model ActivatePast

        "]" ->
          normalMode model ActivateFuture

        "mod+s" ->
          normalMode model
            (CommitChanges 0)

        other ->
          let
            deb = Debug.log "keyboard" other
          in
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

port externals : ((String, String) -> msg) -> Sub msg -- ~ Sub (String, String)
port opsIn : (Json.Encode.Value -> msg) -> Sub msg -- ~ Sub Op

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ externals ExternalCommand
    , opsIn OpIn
    ]




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
