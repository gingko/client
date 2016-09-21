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
    , subscriptions = \_ -> Sub.none
    }


port saveNodes : List Node -> Cmd msg
port saveContents : List Content -> Cmd msg
port saveRoot : String -> Cmd msg
port activateCard : String -> Cmd msg


-- MODEL


type alias Model =
  { contents : List Content
  , nodes : List Node
  , operations : List Msg
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
        newTree = buildStructure data
      in
        { contents = data.contents
        , nodes = data.nodes
        , operations = []
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
          ! [saveNodes newNodes, saveContents newContents, saveRoot newRootId]

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
            ! [ Task.perform (\_ -> NoOp) (\_ -> NoOp) (Dom.focus ("card-edit-" ++ uid)) ]

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
          { model
            | tree = Tree.update (Tree.UpdateCard uid str) model.tree
            , viewState =
                ViewState
                  model.viewState.active
                  Nothing
                  ""
          }
            ! []
        
        Tree.InsertChild uid ->
          { model
            | tree = Tree.update msg model.tree
          }
            ! []

        Tree.InsertBelow uid ->
          { model
            | tree = Tree.update msg model.tree
          }
            ! []

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
        , div [id "app" ]
            ( columns
              |> List.map (viewColumn model.viewState)
              |> List.map (App.map TreeMsg)
            )
        ]
