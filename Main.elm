port module Main exposing (..)


import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Sha1
import Markdown
import String
import List.Extra as ListExtra
import Task


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
port activateCard : Int -> Cmd msg


-- MODEL


type alias Model =
  { contents : List Content
  , nodes : List Node
  , tree : Tree
  , rootId : String
  , active : Int
  , editing : Maybe Int
  , field : String
  }

type alias Content =
  { id : String
  , contentType : String
  , content : String
  }

type alias Node =
  { id : String
  , contentId : String
  , childrenIds : List String
  }

type alias Tree = 
  { uid : Int
  , content : Content
  , children : Children
  }

type alias Data =
  { contents : List Content
  , nodes : List Node
  , rootId : String
  }

type Children = Children (List Tree)
type alias Group = List Tree
type alias Column = List (List Tree)


defaultContent : Content
defaultContent =
  { id = "0"
  , contentType = "text/markdown"
  , content = "defaultContent"
  }


defaultModel : Model
defaultModel =
  { contents = [defaultContent, { defaultContent | id = "1", content = "2" }]
  , nodes = [Node "0" "0" ["1"], Node "1" "1" []]
  , tree = { uid = 0 , content = defaultContent , children = Children [] }
  , rootId = "0"
  , active = 0
  , editing = Nothing
  , field = ""
  }


init : Maybe Data -> ( Model, Cmd Msg )
init savedData =
  case savedData of
    Nothing ->
      defaultModel ! [ ]
    Just data ->
      { contents = data.contents
      , nodes = data.nodes
      , tree = buildStructure data
      , rootId = data.rootId
      , active = 0
      , editing = Nothing
      , field = ""
      }
        ! [ ]





-- UPDATE


type Msg
    = NoOp
    | Activate Int
    | OpenCard Int String
    | CancelCard
    | UpdateField String
    | SaveCard String Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    Activate uid ->
      { model 
        | active = uid
      }
        ! [ activateCard uid ]

    OpenCard uid str ->
      { model
        | active = model.active
        , editing = Just uid 
        , field = str
      }
        ! [ Task.perform (\_ -> NoOp) (\_ -> NoOp) (Dom.focus ("card-edit-" ++ toString uid)) ]

    CancelCard ->
      { model
        | active = model.active
        , editing = Nothing 
        , field = ""
      }
        ! []

    UpdateField str ->
      { model
        | active = model.active
        , editing = model.editing
        , field = str 
      } 
        ! []

    SaveCard str uid ->
      let
        newTree = updateTree (SaveCard str uid) model.tree
        newNodes = 
          (treeToNodes []) newTree
            |> List.filter (\n -> not (List.member n model.nodes))
        newContents = 
          getContents newTree
            |> List.filter (\c -> not (List.member c model.contents))
        newRootId = getId newTree
      in
        { model
          | nodes = model.nodes ++ newNodes
          , contents = model.contents ++ newContents
          , tree = newTree
          , rootId = newRootId
          , active = model.active
          , editing = Nothing
          , field = model.field
        }
          ! [saveNodes newNodes, saveContents newContents, saveRoot newRootId]

-- VIEW


view : Model -> Html Msg
view model =
  let
    columns = getColumns([[[ model.tree ]]])
  in
    div [ id "app" ]
        (List.map (viewColumn model) columns)


viewColumn : Model -> Column -> Html Msg
viewColumn model col =
  div 
    [ class "column" ]
    [ div 
        [ class "buffer" ][]
    , div [](List.map (viewGroup model) col)
    , div
        [ class "buffer" ][]
    ]


viewGroup : Model -> Group -> Html Msg
viewGroup model xs =
  div [ class "group" ]
      (List.map (viewCard model) xs)


viewCard : Model -> Tree -> Html Msg
viewCard model x =
    div [ id ("card-" ++ (toString x.uid))
        , classList [ ("card", True)
                    , ("active", model.active == x.uid)
                    , ("editing", model.editing == Just x.uid)
                    ]
        , onClick (Activate x.uid)
        , onDoubleClick (OpenCard x.uid x.content.content)
        ]
        [ div [ class "view" ] [ Markdown.toHtml [] x.content.content ]
        , textarea
            [ id ( "card-edit-" ++ toString x.uid )
            , class "edit"
            , value model.field
            , onBlur CancelCard
            , onInput UpdateField
            , onEnter (SaveCard model.field x.uid)
            ]
            []
        ]


-- STRUCTURING


getChildren : Tree -> List Tree
getChildren x =
  case x.children of
    Children c ->
      c


nodeToTree : Data -> Int -> Node -> Tree
nodeToTree data uid a =
  let
    fmFunction id = ListExtra.find (\a -> a.id == id) data.nodes -- (String -> Maybe Node)
    imFunction = (\idx -> nodeToTree data (idx + uid + 1))
  in
    { uid = uid
    , content =
        data.contents 
          |> ListExtra.find (\c -> c.id == (a.contentId))
          |> Maybe.withDefault defaultContent
    , children = a.childrenIds -- List String
                  |> List.filterMap fmFunction -- List Node
                  |> List.indexedMap imFunction -- List Tree
                  |> Children
    }


columnHasChildren : Column -> Bool
columnHasChildren col =
  col |> List.concat
      |> List.any (\x -> (getChildren x) /= [])


nextColumn : Column -> Column
nextColumn col =
  (List.map getChildren (List.concat col))


getColumns : List Column -> List Column
getColumns cols =
  let
    col = case (ListExtra.last cols) of
      Nothing -> [[]]
      Just c -> c
    hasChildren = columnHasChildren col
  in
    if hasChildren then
      getColumns(cols ++ [nextColumn(col)])
    else
      cols


buildStructure : Data -> Tree
buildStructure data =
  data.nodes -- List Node
    |> ListExtra.find (\a -> a.id == data.rootId) -- Maybe Node
    |> Maybe.withDefault (Node "0" "0" []) -- Node
    |> nodeToTree data 0 -- Tree





treeToNodes : List Node -> Tree -> List Node
treeToNodes nodes {uid, content, children} =
  case children of
    Children [] ->
      { id = Sha1.sha1(content.id ++ newLine )
      , contentId = content.id
      , childrenIds = []
      } :: nodes

    Children trees ->
      let
        descendants = 
          trees 
            |> List.map (treeToNodes nodes)

        childrenIds =
          trees
            |> List.map treeToNode -- TODO: recursion twice, likely costly unnecessary
            |> List.map .id
      in
        { id = Sha1.sha1(content.id ++ newLine ++ (String.concat childrenIds))
        , contentId = content.id
        , childrenIds = childrenIds
        } :: nodes ++ (List.concat descendants)


treeToNode : Tree -> Node
treeToNode {uid, content, children} =
  case children of
    Children [] ->
      { id = Sha1.sha1(content.id ++ newLine)
      , contentId = content.id
      , childrenIds = []
      }

    Children trees ->
      let
        childrenIds = 
          trees 
            |> List.map treeToNode
            |> List.map .id
      in
        { id = Sha1.sha1(content.id ++ newLine ++ (String.concat childrenIds))
        , contentId = content.id
        , childrenIds = childrenIds
        }


getContents : Tree -> List Content
getContents {uid, content, children} =
  case children of
    Children [] ->
      [content]

    Children trees ->
      [content] ++ (List.concatMap getContents trees)


getId : Tree -> String
getId {uid, content, children} =
  case children of
    Children [] ->
      Sha1.sha1(content.id ++ newLine )

    Children trees ->
      let
        childrenIds =
          trees
            |> List.map getId
      in
        Sha1.sha1(content.id ++ newLine ++ (String.concat childrenIds))


updateTree : Msg -> Tree -> Tree
updateTree msg tree =
  case msg of
    SaveCard str uid ->
      if tree.uid == uid then
         { tree | content = Content (Sha1.sha1 str) "" str }
      else
        case tree.children of
          Children [] ->
            tree
          Children trees ->
            { tree | children = Children (List.map (updateTree (SaveCard str uid)) trees) }

    _ ->
      tree



--HELPERS


newLine : String
newLine =
  String.fromList ['\n']


onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then
        msg
      else NoOp
  in
    on "keydown" (Json.map tagger keyCode)
