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
        , tree = Debug.log "newTree" newTree
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
    | Activate String
    | OpenCard String String
    | CancelCard
    | UpdateField String
    | UpdateCard String String
    | InsertBelow String
    | DeleteCard String
    | SaveTree


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    Activate uid ->
      { model
        | viewState = ViewState uid model.viewState.editing model.viewState.field
      }
        ! [ activateCard uid ]

    OpenCard uid str ->
      { model
        | viewState =
            ViewState
              model.viewState.active
              (Just uid)
              str
      }
        ! [ Task.perform (\_ -> NoOp) (\_ -> NoOp) (Dom.focus ("card-edit-" ++ uid)) ]

    CancelCard ->
      { model
        | viewState =
            ViewState
              model.viewState.active
              Nothing
              ""
      }
        ! []

    UpdateField str ->
      { model
        | viewState =
            ViewState
              model.viewState.active
              model.viewState.editing
              str
      }
        ! []

    UpdateCard uid str ->
      { model
        | tree = Tree.update (Tree.UpdateCard uid str) model.tree
        , viewState =
            ViewState
              model.viewState.active
              Nothing
              ""
        , operations = Debug.log "ops" ((UpdateCard uid str) :: model.operations )
      }
        ! []

    InsertBelow uid ->
      { model
        | tree = Tree.update (Tree.InsertBelow (Debug.log "uid" uid)) model.tree
      }
        ! []

    DeleteCard uid ->
      { model
        | tree = Tree.update (Tree.DeleteCard uid) model.tree
      }
        ! []


    SaveTree ->
      let
        newNodes =
          (treeToNodes []) model.tree
            |> List.filter (\n -> not (List.member n model.nodes))
        newContents =
          getContents model.tree
            |> List.filter (\c -> not (List.member c model.contents))
        newRootId = getId model.tree
      in
        { model
          | nodes = model.nodes ++ newNodes
          , contents = model.contents ++ newContents
          , operations = []
          , rootId = newRootId
        }
          ! [saveNodes newNodes, saveContents newContents, saveRoot newRootId]



-- VIEW


view : Model -> Html Msg
view model =
  let
    columns = getColumns([[[ model.tree ]]])
  in
    div [ id "wrapper" ]
        [ button [onClick SaveTree][text "save"]
        , div [id "app" ](List.map (viewColumn model.viewState) columns)
        ]


viewColumn : ViewState -> Column -> Html Msg
viewColumn model col =
  div
    [ class "column" ]
    [ div
        [ class "buffer" ][]
    , div [](List.map (viewGroup model) col)
    , div
        [ class "buffer" ][]
    ]


viewGroup : ViewState -> Group -> Html Msg
viewGroup model xs =
  div [ class "group" ]
      (List.map (viewCard model) xs)


viewCard : ViewState -> Tree -> Html Msg
viewCard model tree =
    div [ id ("card-" ++ tree.uid)
        , classList [ ("card", True)
                    , ("active", model.active == tree.uid)
                    , ("editing", model.editing == Just tree.uid)
                    ]
        , onClick (Activate tree.uid)
        , onDoubleClick (OpenCard tree.uid tree.content.content)
        ]
        [ div [ class "view" ] [ Markdown.toHtml [] tree.content.content ]
        , button [ onClick (DeleteCard tree.uid) ][text "x"]
        , textarea
            [ id ( "card-edit-" ++ tree.uid )
            , class "edit"
            , value model.field
            , onBlur CancelCard
            , onInput UpdateField
            , onEnter (UpdateCard tree.uid model.field)
            ]
            []
        , button [ onClick (InsertBelow tree.uid) ][text "+"]
        ]


-- STRUCTURING

onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then
        msg
      else NoOp
  in
    on "keydown" (Json.map tagger keyCode)
