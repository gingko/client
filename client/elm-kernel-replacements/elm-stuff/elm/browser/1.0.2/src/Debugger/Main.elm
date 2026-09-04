module Debugger.Main exposing
  ( cornerView
  , getUserModel
  , initialWindowHeight
  , initialWindowWidth
  , popoutView
  , wrapInit
  , wrapSubs
  , wrapUpdate
  )


import Bitwise
import Debugger.Expando as Expando exposing (Expando)
import Debugger.History as History exposing (History)
import Debugger.Metadata as Metadata exposing (Metadata)
import Debugger.Overlay as Overlay
import Debugger.Report as Report
import Dict exposing (Dict)
import Elm.Kernel.Debugger
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (on, onClick, onInput, onMouseDown, onMouseUp)
import Html.Lazy exposing (lazy)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)
import VirtualDom as V



-- CONSTANTS


minimumPanelSize : Int
minimumPanelSize =
  150


initialWindowWidth : Int
initialWindowWidth =
  900


initialWindowHeight : Int
initialWindowHeight =
  420



-- SUBSCRIPTIONS


wrapSubs : (model -> Sub msg) -> Model model msg -> Sub (Msg msg)
wrapSubs subscriptions model =
  Sub.map UserMsg (subscriptions (getLatestModel model.state))



-- MODEL


type alias Model model msg =
  { history : History model msg
  , state : State model msg
  , expandoModel : Expando
  , expandoMsg : Expando
  , metadata : Result Metadata.Error Metadata
  , overlay : Overlay.State
  , popout : Popout
  , layout : Layout
  }


type Popout
  = Popout Popout


type Layout
  = Vertical DragStatus Float Float
  | Horizontal DragStatus Float Float


type DragStatus
  = Static
  | Moving


getUserModel : Model model msg -> model
getUserModel model =
  getCurrentModel model.state



-- STATE


type State model msg
  = Running model
  | Paused Int model model msg (History model msg)


getLatestModel : State model msg -> model
getLatestModel state =
  case state of
    Running model ->
      model

    Paused _ _ model _ _ ->
      model


getCurrentModel : State model msg -> model
getCurrentModel state =
  case state of
    Running model ->
      model

    Paused _ model _ _ _ ->
      model


isPaused : State model msg -> Bool
isPaused state =
  case state of
    Running _ ->
      False

    Paused _ _ _ _ _ ->
      True


cachedHistory : Model model msg -> History model msg
cachedHistory model =
  case model.state of
    Running _ ->
      model.history

    Paused _ _ _ _ history ->
      history



-- INIT


wrapInit : Encode.Value -> Popout -> (flags -> ( model, Cmd msg )) -> flags -> ( Model model msg, Cmd (Msg msg) )
wrapInit metadata popout init flags =
  let
    (userModel, userCommands) = init flags
  in
  ( { history = History.empty userModel
    , state = Running userModel
    , expandoModel = Expando.init userModel
    , expandoMsg = Expando.init ()
    , metadata = Metadata.decode metadata
    , overlay = Overlay.none
    , popout = popout
    , layout = Horizontal Static 0.3 0.5
    }
  , Cmd.map UserMsg userCommands
  )



-- UPDATE


type Msg msg
    = NoOp
    | UserMsg msg
    | TweakExpandoMsg Expando.Msg
    | TweakExpandoModel Expando.Msg
    | Pause
    | Resume
    | Jump Int
    | SliderJump Int
    | Open
    | Up
    | Down
    | Import
    | Export
    | Upload String
    | OverlayMsg Overlay.Msg
    --
    | SwapLayout
    | DragStart
    | Drag DragInfo
    | DragEnd


type alias UserUpdate model msg =
    msg -> model -> ( model, Cmd msg )


wrapUpdate : UserUpdate model msg -> Msg msg -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
wrapUpdate update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    UserMsg userMsg ->
        let
          userModel = getLatestModel model.state
          newHistory = History.add userMsg userModel model.history
          ( newUserModel, userCmds ) = update userMsg userModel
          commands = Cmd.map UserMsg userCmds
        in
        case model.state of
          Running _ ->
            ( { model
                | history = newHistory
                , state = Running newUserModel
                , expandoModel = Expando.merge newUserModel model.expandoModel
                , expandoMsg = Expando.merge userMsg model.expandoMsg
              }
            , Cmd.batch [ commands, scroll model.popout ]
            )

          Paused index indexModel _ _ history ->
            ( { model
                | history = newHistory
                , state = Paused index indexModel newUserModel userMsg history
              }
            , commands
            )

    TweakExpandoMsg eMsg ->
      ( { model | expandoMsg = Expando.update eMsg model.expandoMsg }
      , Cmd.none
      )

    TweakExpandoModel eMsg ->
      ( { model | expandoModel = Expando.update eMsg model.expandoModel }
      , Cmd.none
      )

    Pause ->
      case model.state of
          Running _ ->
              let
                size = History.size model.history
              in
              if size == 0 then
                ( model, Cmd.none )
              else
                ( jumpUpdate update (size - 1) model
                , Cmd.none
                )

          Paused _ _ userModel userMsg _ ->
              ( model, Cmd.none )

    Resume ->
        case model.state of
            Running _ ->
                ( model, Cmd.none )

            Paused _ _ userModel userMsg _ ->
                ( { model
                    | state = Running userModel
                    , expandoMsg = Expando.merge userMsg model.expandoMsg
                    , expandoModel = Expando.merge userModel model.expandoModel
                  }
                , scroll model.popout
                )

    Jump index ->
      ( jumpUpdate update index model
      , Cmd.none
      )

    SliderJump index ->
      ( jumpUpdate update index model
      , scrollTo (History.idForMessageIndex index) model.popout
      )

    Open ->
      ( model
      , Task.perform (always NoOp) (Elm.Kernel.Debugger.open model.popout)
      )

    Up ->
      case model.state of
        Running _ ->
          ( model, Cmd.none )

        Paused i _ _ _ history ->
          let
            targetIndex = i + 1
          in
          if targetIndex < History.size history then
              wrapUpdate update (SliderJump targetIndex) model
          else
              wrapUpdate update Resume model

    Down ->
      case model.state of
        Running _ ->
          wrapUpdate update (Jump (History.size model.history - 1)) model

        Paused index _ _ _ _ ->
          if index > 0 then
            wrapUpdate update (SliderJump (index - 1)) model
          else
            ( model, Cmd.none )

    Import ->
      withGoodMetadata model <|
        \_ -> ( model, upload model.popout )

    Export ->
      withGoodMetadata model <|
        \metadata -> ( model, download metadata model.history )

    Upload jsonString ->
      withGoodMetadata model <|
        \metadata ->
          case Overlay.assessImport metadata jsonString of
            Err newOverlay ->
              ( { model | overlay = newOverlay }, Cmd.none )

            Ok rawHistory ->
              loadNewHistory rawHistory update model

    OverlayMsg overlayMsg ->
      case Overlay.close overlayMsg model.overlay of
        Nothing ->
          ( { model | overlay = Overlay.none }, Cmd.none )

        Just rawHistory ->
          loadNewHistory rawHistory update model

    SwapLayout ->
      ( { model | layout = swapLayout model.layout }, Cmd.none )

    DragStart ->
      ( { model | layout = setDragStatus Moving model.layout }, Cmd.none )

    Drag info ->
      ( { model | layout = drag info model.layout }, Cmd.none )

    DragEnd ->
      ( { model | layout = setDragStatus Static model.layout }, Cmd.none )


jumpUpdate : UserUpdate model msg -> Int -> Model model msg -> Model model msg
jumpUpdate update index model =
    let
        history =
            cachedHistory model

        currentMsg =
            History.getRecentMsg history

        currentModel =
            getLatestModel model.state

        ( indexModel, indexMsg ) =
            History.get update index history
    in
    { model
        | state = Paused index indexModel currentModel currentMsg history
        , expandoModel = Expando.merge indexModel model.expandoModel
        , expandoMsg = Expando.merge indexMsg model.expandoMsg
    }



-- LAYOUT HELPERS


swapLayout : Layout -> Layout
swapLayout layout =
  case layout of
    Horizontal s x y -> Vertical s x y
    Vertical s x y -> Horizontal s x y


setDragStatus : DragStatus -> Layout -> Layout
setDragStatus status layout =
  case layout of
    Horizontal _ x y -> Horizontal status x y
    Vertical   _ x y -> Vertical   status x y


drag : DragInfo -> Layout -> Layout
drag info layout =
  case layout of
    Horizontal status _ y ->
      Horizontal status (info.x / info.width) y

    Vertical status x _ ->
      Vertical status x (info.y / info.height)



-- COMMANDS


scroll : Popout -> Cmd (Msg msg)
scroll popout =
  Task.perform (always NoOp) (Elm.Kernel.Debugger.scroll popout)


scrollTo : String -> Popout -> Cmd (Msg msg)
scrollTo id popout =
  Task.perform (always NoOp) (Elm.Kernel.Debugger.scrollTo id popout)


upload : Popout -> Cmd (Msg msg)
upload popout =
  Task.perform Upload (Elm.Kernel.Debugger.upload popout)


download : Metadata -> History model msg -> Cmd (Msg msg)
download metadata history =
  let
    historyLength = History.size history
  in
  Task.perform (\_ -> NoOp) <| Elm.Kernel.Debugger.download historyLength <|
    Elm.Kernel.Json.unwrap <|
      Encode.object
        [ ( "metadata", Metadata.encode metadata )
        , ( "history", History.encode history )
        ]



-- UPDATE OVERLAY


withGoodMetadata :
    Model model msg
    -> (Metadata -> ( Model model msg, Cmd (Msg msg) ))
    -> ( Model model msg, Cmd (Msg msg) )
withGoodMetadata model func =
  case model.metadata of
    Ok metadata ->
      func metadata

    Err error ->
      ( { model | overlay = Overlay.badMetadata error }
      , Cmd.none
      )


loadNewHistory : Encode.Value -> UserUpdate model msg -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
loadNewHistory rawHistory update model =
    let
        initialUserModel =
            History.getInitialModel model.history

        pureUserUpdate msg userModel =
            Tuple.first (update msg userModel)

        decoder =
            History.decoder initialUserModel pureUserUpdate
    in
    case Decode.decodeValue decoder rawHistory of
        Err _ ->
            ( { model | overlay = Overlay.corruptImport }
            , Cmd.none
            )

        Ok ( latestUserModel, newHistory ) ->
            ( { model
                | history = newHistory
                , state = Running latestUserModel
                , expandoModel = Expando.init latestUserModel
                , expandoMsg = Expando.init (History.getRecentMsg newHistory)
                , overlay = Overlay.none
              }
            , Cmd.none
            )



-- CORNER VIEW


cornerView : Model model msg -> Html (Msg msg)
cornerView model =
  Overlay.view
    { resume = Resume
    , open = Open
    , importHistory = Import
    , exportHistory = Export
    , wrap = OverlayMsg
    }
    (isPaused model.state)
    (Elm.Kernel.Debugger.isOpen model.popout)
    (History.size model.history)
    model.overlay


toBlockerType : Model model msg -> Overlay.BlockerType
toBlockerType model =
  Overlay.toBlockerType (isPaused model.state) model.overlay



-- BIG DEBUG VIEW


popoutView : Model model msg -> Html (Msg msg)
popoutView model =
  let
    maybeIndex =
      case model.state of
        Running _            -> Nothing
        Paused index _ _ _ _ -> Just index

    historyToRender = cachedHistory model
  in
  node "body"
    (
      toDragListeners model.layout
      ++
      [ style "margin" "0"
      , style "padding" "0"
      , style "width" "100%"
      , style "height" "100%"
      , style "font-family" "monospace"
      , style "display" "flex"
      , style "background-color" "white"
      , style "flex-direction" (toFlexDirection model.layout)
      ]
    )
    [ viewHistory maybeIndex historyToRender model.layout
    , viewDragZone model.layout
    , viewExpando model.expandoMsg model.expandoModel model.layout
    ]


toFlexDirection : Layout -> String
toFlexDirection layout =
  case layout of
    Horizontal _ _ _ -> "row"
    Vertical   _ _ _ -> "column-reverse"



-- DRAG LISTENERS


toDragListeners : Layout -> List (Attribute (Msg msg))
toDragListeners layout =
  case getDragStatus layout of
    Static -> []
    Moving -> [ onMouseMove, onMouseUp DragEnd ]


getDragStatus : Layout -> DragStatus
getDragStatus layout =
  case layout of
    Horizontal status _ _ -> status
    Vertical   status _ _ -> status


type alias DragInfo =
  { x : Float
  , y : Float
  , down : Bool
  , width : Float
  , height : Float
  }


onMouseMove : Attribute (Msg msg)
onMouseMove =
  on "mousemove" <| Decode.map Drag <|
    Decode.map5 DragInfo
      (Decode.field "pageX" Decode.float)
      (Decode.field "pageY" Decode.float)
      (Decode.field "buttons" (Decode.map (\v -> v == 1) Decode.int))
      (decodeDimension "innerWidth")
      (decodeDimension "innerHeight")


decodeDimension : String -> Decoder Float
decodeDimension field =
  Decode.at [ "currentTarget", "ownerDocument", "defaultView", field ] Decode.float



-- VIEW DRAG ZONE


viewDragZone : Layout -> Html (Msg msg)
viewDragZone layout =
  case layout of
    Horizontal _ x _ ->
      div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" (toPercent x)
        , style "margin-left" "-5px"
        , style "width" "10px"
        , style "height" "100%"
        , style "cursor" "col-resize"
        , onMouseDown DragStart
        ]
        []

    Vertical _ _ y ->
      div
        [ style "position" "absolute"
        , style "top" (toPercent y)
        , style "left" "0"
        , style "margin-top" "-5px"
        , style "width" "100%"
        , style "height" "10px"
        , style "cursor" "row-resize"
        , onMouseDown DragStart
        ]
        []



-- LAYOUT HELPERS


toPercent : Float -> String
toPercent fraction =
  String.fromFloat (100 * fraction) ++ "%"


toMouseBlocker : Layout -> String
toMouseBlocker layout =
  case getDragStatus layout of
    Static -> "auto"
    Moving -> "none"



-- VIEW HISTORY


viewHistory : Maybe Int -> History model msg -> Layout -> Html (Msg msg)
viewHistory maybeIndex history layout =
  let
    (w,h) = toHistoryPercents layout
    block = toMouseBlocker layout
  in
  div
    [ style "width" w
    , style "height" h
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "color" "#DDDDDD"
    , style "background-color" "rgb(61, 61, 61)"
    , style "pointer-events" block
    , style "user-select" block
    ]
    [ viewHistorySlider history maybeIndex
    , Html.map Jump (History.view maybeIndex history)
    , lazy viewHistoryOptions layout
    ]


toHistoryPercents : Layout -> (String, String)
toHistoryPercents layout =
  case layout of
    Horizontal _ x _ -> ( toPercent x, "100%" )
    Vertical   _ _ y -> ( "100%", toPercent (1 - y) )


viewHistorySlider : History model msg -> Maybe Int -> Html (Msg msg)
viewHistorySlider history maybeIndex =
  let
    lastIndex = History.size history - 1
    selectedIndex = Maybe.withDefault lastIndex maybeIndex
  in
  div
    [ style "display" "flex"
    , style "flex-direction" "row"
    , style "align-items" "center"
    , style "width" "100%"
    , style "height" "36px"
    , style "background-color" "rgb(50, 50, 50)"
    ]
    [ lazy viewPlayButton (isPlaying maybeIndex)
    , input
        [ type_ "range"
        , style "width" "calc(100% - 56px)"
        , style "height" "36px"
        , style "margin" "0 10px"
        , A.min "0"
        , A.max (String.fromInt lastIndex)
        , value (String.fromInt selectedIndex)
        , onInput (String.toInt >> Maybe.withDefault lastIndex >> SliderJump)
        ]
        []
    ]


viewPlayButton : Bool -> Html (Msg msg)
viewPlayButton playing =
  button
    [ style "background" "#1293D8"
    , style "border" "none"
    , style "color" "white"
    , style "cursor" "pointer"
    , style "width" "36px"
    , style "height" "36px"
    , onClick (if playing then Pause else Resume)
    ]
    [ if playing
      then icon "M2 2h4v12h-4v-12z M10 2h4v12h-4v-12z"
      else icon "M2 2l12 7l-12 7z"
    ]


isPlaying : Maybe Int -> Bool
isPlaying maybeIndex =
  case maybeIndex of
    Nothing -> True
    Just _ -> False


viewHistoryOptions : Layout -> Html (Msg msg)
viewHistoryOptions layout =
  div
    [ style "width" "100%"
    , style "height" "36px"
    , style "display" "flex"
    , style "flex-direction" "row"
    , style "align-items" "center"
    , style "justify-content" "space-between"
    , style "background-color" "rgb(50, 50, 50)"
    ]
    [ viewHistoryButton "Swap Layout" SwapLayout (toHistoryIcon layout)
    , div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "align-items" "center"
        , style "justify-content" "space-between"
        ]
        [ viewHistoryButton "Import" Import "M5 1a1 1 0 0 1 0 2h-2a1 1 0 0 0-1 1v8a1 1 0 0 0 1 1h10a1 1 0 0 0 1-1a1 1 0 0 1 2 0a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3v-8a3 3 0 0 1 3-3z M10 2a1 1 0 0 0 -2 0v6a1 1 0 0 0 1 1h6a1 1 0 0 0 0-2h-3.586l4.293-4.293a1 1 0 0 0-1.414-1.414l-4.293 4.293z"
        , viewHistoryButton "Export" Export "M5 1a1 1 0 0 1 0 2h-2a1 1 0 0 0-1 1v8a1 1 0 0 0 1 1h10a1 1 0 0 0 1-1 a1 1 0 0 1 2 0a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3v-8a3 3 0 0 1 3-3z M9 3a1 1 0 1 1 0-2h6a1 1 0 0 1 1 1v6a1 1 0 1 1-2 0v-3.586l-5.293 5.293 a1 1 0 0 1-1.414-1.414l5.293 -5.293z"
        ]
    ]


viewHistoryButton : String -> msg -> String -> Html msg
viewHistoryButton label msg path =
  button
    [ style "display" "flex"
    , style "flex-direction" "row"
    , style "align-items" "center"
    , style "background" "none"
    , style "border" "none"
    , style "color" "inherit"
    , style "cursor" "pointer"
    , onClick msg
    ]
    [ icon path
    , span [ style "padding-left" "6px" ] [ text label ]
    ]


icon : String -> Html msg
icon path =
  V.nodeNS "http://www.w3.org/2000/svg" "svg"
    [ V.attribute "viewBox" "0 0 16 16"
    , V.attribute "xmlns" "http://www.w3.org/2000/svg"
    , V.attribute "fill" "currentColor"
    , V.attribute "width" "16px"
    , V.attribute "height" "16px"
    ]
    [ V.nodeNS "http://www.w3.org/2000/svg" "path"
        [ V.attribute "d" path ]
        []
    ]


toHistoryIcon : Layout -> String
toHistoryIcon layout =
  case layout of
    Horizontal _ _ _ -> "M13 1a3 3 0 0 1 3 3v8a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3v-8a3 3 0 0 1 3-3z M13 3h-10a1 1 0 0 0-1 1v5h12v-5a1 1 0 0 0-1-1z M14 10h-12v2a1 1 0 0 0 1 1h10a1 1 0 0 0 1-1z"
    Vertical   _ _ _ -> "M0 4a3 3 0 0 1 3-3h10a3 3 0 0 1 3 3v8a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3z M2 4v8a1 1 0 0 0 1 1h2v-10h-2a1 1 0 0 0-1 1z M6 3v10h7a1 1 0 0 0 1-1v-8a1 1 0 0 0-1-1z"



-- VIEW EXPANDO


viewExpando : Expando -> Expando -> Layout -> Html (Msg msg)
viewExpando expandoMsg expandoModel layout =
  let
    (w,h) = toExpandoPercents layout
    block = toMouseBlocker layout
  in
  div
    [ style "display" "block"
    , style "width" ("calc(" ++ w ++ " - 4em)")
    , style "height" ("calc(" ++ h ++ " - 4em)")
    , style "padding" "2em"
    , style "margin" "0"
    , style "overflow" "auto"
    , style "pointer-events" block
    , style "-webkit-user-select" block
    , style "-moz-user-select" block
    , style "-ms-user-select" block
    , style "user-select" block
    ]
    [ div [ style "color" "#ccc", style "padding" "0 0 1em 0" ] [ text "-- MESSAGE" ]
    , Html.map TweakExpandoMsg <| Expando.view [] expandoMsg
    , div [ style "color" "#ccc", style "padding" "1em 0" ] [ text "-- MODEL" ]
    , Html.map TweakExpandoModel <| Expando.view [] expandoModel
    ]


toExpandoPercents : Layout -> (String, String)
toExpandoPercents layout =
  case layout of
    Horizontal _ x _ -> ( toPercent (1 - x), "100%" )
    Vertical   _ _ y -> ( "100%", toPercent y )


-- No case/let/if in parentheses. Always new top-level function.
-- No case/let/if within a let. Always new top-level function.
-- Replace "creating variable to use in different branches" with "same function call in different branches"
