module Toast exposing
    ( persistent, expireIn, expireOnBlur
    , withExitTransition
    , Toast, Tray, tray
    , add, addUnique, addUniqueBy, addUniqueWith
    , Msg, update, tuple
    , Config, config, render
    , Phase(..), Interaction(..), Info
    , withAttributes, withFocusAttributes
    , withEnterAttributes, withExitAttributes, withTransitionAttributes
    , withTrayAttributes, withTrayNode
    , remove, exit
    , filter
    )

{-| All you need to create, append and render toast stacks
in the Elm architecture.


# Pick one kind of toast

@docs persistent, expireIn, expireOnBlur


# Set an exit transition length

@docs withExitTransition


# Start with an empty tray, add your toasts

@docs Toast, Tray, tray
@docs add, addUnique, addUniqueBy, addUniqueWith


# Forward messages and update toast's tray

@docs Msg, update, tuple


# Customize & render toasts

@docs Config, config, render
@docs Phase, Interaction, Info
@docs withAttributes, withFocusAttributes
@docs withEnterAttributes, withExitAttributes, withTransitionAttributes
@docs withTrayAttributes, withTrayNode


# Remove toasts

@docs remove, exit

-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Process
import Task


type Private a
    = Private a


type alias Toast_ content =
    { id : Id
    , blurCount : Int
    , phase : Phase
    , interaction : Interaction
    , lifespan : Lifespan
    , exitTransition : Int
    , content : content
    }


{-| `Toast.Toast` is something you'll need if you have to reference
the output type of [persistent](#persistent), [expireIn](#expireIn), [expireOnBlur](#expireOnBlur),
this is one of those things you'll know when you need it, so don't worry about this.
-}
type alias Toast content =
    Private (Toast_ content)


type alias Id =
    Int


{-| A toast go through three phases:

  - `Toast.Enter` when it's just been added to a tray
  - `Toast.In` during the next render, immediately after enter phase
  - `Toast.Exit` when it's about to be removed

You can control how much time a toast is kept in
`Toast.Exit` phase through [withExitTransition](#withExitTransition).

Both `Toast.Enter` and `Toast.Exit` are considered [transition phases](#withTransitionAttributes).

-}
type Phase
    = Enter
    | In
    | Exit


{-| Can be `Toast.Focus` or `Toast.Blur`, just like [Toast.Phase](#Phase)
you'll have this information while rendering a toast through [Toast.Info](#Info).

    viewToast :
        List (Html.Attribute Msg)
        -> Toast.Info Toast
        -> Html Msg
    viewToast attributes toast =
        Html.div
            (if toast.interaction == Toast.Focus then
                class "toast-active" :: attributes

             else
                attributes
            )
            [ Html.text toast.content.message ]

    view : Model -> Html Msg
    view model =
        Toast.config ToastMsg
            |> Toast.render viewToast model.tray

-}
type Interaction
    = Focus
    | Blur


type Lifespan
    = Persistent
    | ExpireIn Int
    | ExpireOnBlur Int


new : Lifespan -> content -> Toast content
new lifespan content =
    Private
        { id = -1
        , blurCount = 0
        , phase = Enter
        , interaction = Blur
        , lifespan = lifespan
        , exitTransition = 0
        , content = content
        }


{-| Create a new toast that won't be automatically removed,
it will stay visible until you explicitly remove it.

    Toast.persistent
        { message = "hello, world"
        , color = "#7f7"
        }

-}
persistent : content -> Toast content
persistent =
    new Persistent


{-| Create a new toast with a fixed expiry.
Toast's lifespan is expressed in milliseconds.

    Toast.expireIn 5000 "I'll disappear in five seconds"

-}
expireIn : Int -> content -> Toast content
expireIn ttl =
    new (ExpireIn ttl)


{-| This kind of toast has an interaction-based expiration.
It'll be removed automatically in given time if user doesn't
interact with the toast, but it'll stay visible if receives focus
or mouse over.

When the interaction has ended and the toast lost both focus
and mouse over the given amount of milliseconds is awaited before
it's removed.

    Toast.expireOnBlur 5000 "I won't go away while I'm focused!"

-}
expireOnBlur : Int -> content -> Toast content
expireOnBlur ttl =
    new (ExpireOnBlur ttl)


{-| Add a delay between toast exit phase and actual removal.

    Toast.persistent { message = "hello, world", color = "#7f7" }
        |> Toast.withExitTransition 1000

-}
withExitTransition : Int -> Toast content -> Toast content
withExitTransition ttl (Private toast) =
    Private { toast | exitTransition = ttl }


type alias Tray_ content =
    { currentId : Id
    , toasts : List (Toast_ content)
    }


{-| `Toast.Tray` represents the stack where toasts are stored.
You probably want to use this opaque type in your model:

    type alias MyToast =
        { message : String
        , color : String
        }

    type alias Model =
        { tray : Toast.Tray MyToast

        -- model fields...
        }

-}
type alias Tray content =
    Private (Tray_ content)


{-| An empty tray, it's a thing you can put in an `init`.

    init : anything -> ( Model, Cmd msg )
    init _ =
        ( { tray = Toast.tray }, Cmd.none )

-}
tray : Tray content
tray =
    Private
        { currentId = 0
        , toasts = []
        }


fire : msg -> Cmd msg
fire msg =
    Task.perform identity (Task.succeed msg)


delay : Int -> msg -> Cmd msg
delay ms msg =
    Task.perform (always msg) (Process.sleep <| toFloat ms)


withoutCmd : model -> ( model, Cmd msg )
withoutCmd model =
    ( model, Cmd.none )


withCmds : List (Cmd msg) -> model -> ( model, Cmd msg )
withCmds cmds model =
    ( model, Cmd.batch cmds )


{-| Internal message, you probably want to do something like

    type Msg
        = ToastMsg Toast.Msg
          -- other stuff...
        | AddToast MyToastContent

in your app `Msg`.

-}
type Msg
    = Transition Phase Id
    | Interaction Interaction Id
    | PrepareExit Id
    | Remove Id


onEnter : Id -> Lifespan -> List (Cmd Msg)
onEnter id lifespan =
    case lifespan of
        Persistent ->
            []

        ExpireIn ttl ->
            [ delay ttl (Transition Exit id) ]

        ExpireOnBlur ttl ->
            [ delay ttl (PrepareExit id) ]


internalAdd : Tray_ content -> Toast_ content -> ( Tray content, Cmd Msg )
internalAdd model toast =
    let
        id : Id
        id =
            model.currentId

        toasts : List (Toast_ content)
        toasts =
            model.toasts ++ [ { toast | id = id } ]
    in
    { model | currentId = id + 1, toasts = toasts }
        |> Private
        |> withCmds (delay 100 (Transition In id) :: onEnter id toast.lifespan)


{-| Add a toast to a tray, produces an updated tray and a `Cmd Toast.Msg`.

    updateTuple :
        ( Toast.Tray { message : String, color : String }
        , Cmd Toast.Msg
        )
    updateTuple =
        Toast.persistent { message = "hello, world", color = "#7f7" }
            |> Toast.withExitTransition 1000
            |> Toast.add currentTray

-}
add : Tray content -> Toast content -> ( Tray content, Cmd Msg )
add (Private model) (Private toast) =
    internalAdd model toast


{-| Add a toast only if its content is not already in the tray.

Toast contents are compared with [structural equality](https://package.elm-lang.org/packages/elm/core/latest/Basics#==).

    -- if currentTray already contains a toast with the same
    -- message and color it won't be added again
    Toast.persistent { message = "hello, world", color = "#7f7" }
        |> Toast.addUnique currentTray

-}
addUnique :
    Tray content
    -> Toast content
    -> ( Tray content, Cmd Msg )
addUnique =
    addUniqueWith (==)


{-| This is what you need if, for example, you want to have toast
with unique `content.message`.

    -- no two "hello, world" in the same tray
    Toast.persistent { message = "hello, world", color = "#7f7" }
        |> Toast.addUniqueBy .message currentTray

-}
addUniqueBy :
    (content -> a)
    -> Tray content
    -> Toast content
    -> ( Tray content, Cmd Msg )
addUniqueBy cmp =
    addUniqueWith (\x y -> cmp x == cmp y)


{-| Most powerful `addUnique` version: it takes a function that
compares two toast contents.

    type alias MyToast =
        { message : String
        , color : String
        }

    sameMessageLength : MyToast -> MyToast -> Bool
    sameMessageLength t1 t2 =
        String.length t1.message == String.length t2.message

    -- we can't have two toast with same message length
    -- for some reason...
    Toast.persistent { message = "hello, world", color = "#7f7" }
        |> Toast.addUniqueWith sameMessageLength currentTray

-}
addUniqueWith :
    (content -> content -> Bool)
    -> Tray content
    -> Toast content
    -> ( Tray content, Cmd Msg )
addUniqueWith cmp (Private model) (Private toast) =
    if List.any (.content >> cmp toast.content) model.toasts then
        withoutCmd (Private model)

    else
        internalAdd model toast


filter : (content -> Bool) -> Tray content -> Tray content
filter cmp (Private model) =
    { model | toasts = List.filter (\toast -> cmp toast.content) model.toasts }
        |> Private


setToasts : { model | toasts : t } -> t -> { model | toasts : t }
setToasts model toasts =
    { model | toasts = toasts }


removeToast : Id -> Tray_ content -> ( Tray_ content, Cmd msg )
removeToast id model =
    model.toasts
        |> List.filter (\toast -> toast.id /= id)
        |> setToasts model
        |> withoutCmd


findToastAndUpdate :
    (model -> Maybe ( model, cmd ))
    -> List model
    -> List model
    -> ( List model, Maybe cmd )
findToastAndUpdate updater dest src =
    case src of
        hd :: tl ->
            case updater hd of
                Just ( toast, cmd ) ->
                    ( List.reverse dest ++ toast :: tl, Just cmd )

                Nothing ->
                    findToastAndUpdate updater (hd :: dest) tl

        [] ->
            ( List.reverse dest, Nothing )


updateToastWithCmd :
    (Toast_ content -> ( Toast_ content, Cmd Msg ))
    -> Id
    -> Tray_ content
    -> ( Tray_ content, Cmd Msg )
updateToastWithCmd updater id model =
    let
        doUpdate : Toast_ content -> Maybe ( Toast_ content, Cmd Msg )
        doUpdate toast =
            if toast.id == id then
                Just (updater toast)

            else
                Nothing
    in
    model.toasts
        |> findToastAndUpdate doUpdate []
        |> Tuple.mapBoth (setToasts model) (Maybe.withDefault Cmd.none)


updateToast :
    (Toast_ content -> Toast_ content)
    -> Id
    -> Tray_ content
    -> ( Tray_ content, Cmd Msg )
updateToast updater =
    updateToastWithCmd (updater >> withoutCmd)


onBlur : Toast_ content -> List (Cmd Msg)
onBlur toast =
    case toast.lifespan of
        ExpireOnBlur ttl ->
            [ delay ttl (PrepareExit toast.id) ]

        _ ->
            []


handleBlur : Toast_ content -> ( Toast_ content, Cmd Msg )
handleBlur toast =
    { toast | interaction = Blur, blurCount = toast.blurCount + 1 }
        |> withCmds (onBlur toast)


handlePrepareExit : Toast_ content -> ( Toast_ content, Cmd Msg )
handlePrepareExit toast =
    if toast.interaction == Focus || toast.blurCount > 0 then
        withoutCmd { toast | blurCount = toast.blurCount - 1 }

    else
        ( toast, fire (Transition Exit toast.id) )


handleStartExit : Toast_ content -> ( Toast_ content, Cmd Msg )
handleStartExit toast =
    ( { toast | phase = Exit }, delay toast.exitTransition (Remove toast.id) )


internalUpdate : Msg -> Tray_ content -> ( Tray_ content, Cmd Msg )
internalUpdate msg =
    case msg of
        Transition In id ->
            updateToast (\toast -> { toast | phase = In }) id

        Transition Enter id ->
            updateToast (\toast -> { toast | phase = Enter }) id

        Transition Exit id ->
            updateToastWithCmd handleStartExit id

        Interaction Focus id ->
            updateToast (\toast -> { toast | interaction = Focus }) id

        Interaction Blur id ->
            updateToastWithCmd handleBlur id

        PrepareExit id ->
            updateToastWithCmd handlePrepareExit id

        Remove id ->
            removeToast id


{-| Nothing fancy here: given a [Toast.Msg](#Msg) and a [Toast.Tray](#Tray)
updates tray's state and produces a `Cmd`.
-}
update : Msg -> Tray content -> ( Tray content, Cmd Msg )
update msg (Private model) =
    Tuple.mapFirst Private (internalUpdate msg model)


{-| Helps in conversion between
`( Toast.Tray, Cmd Toast.Msg )` and `( Model, Cmd Msg )`.

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            AddToast content ->
                Toast.persistent content
                    |> Toast.add model.tray
                    |> Toast.tuple ToastMsg model

            ToastMsg tmsg ->
                Toast.update tmsg model.tray
                    |> Toast.tuple ToastMsg model

-}
tuple :
    (Msg -> msg)
    -> { model | tray : Tray content }
    -> ( Tray content, Cmd Msg )
    -> ( { model | tray : Tray content }, Cmd msg )
tuple toAppMsg appModel ( model, cmd ) =
    ( { appModel | tray = model }, Cmd.map toAppMsg cmd )


type alias Config_ msg =
    { toAppMsg : Msg -> msg
    , enterAttributes : List (Html.Attribute msg)
    , exitAttributes : List (Html.Attribute msg)
    , focusAttributes : List (Html.Attribute msg)
    , toastAttributes : List (Html.Attribute msg)
    , trayAttributes : List (Html.Attribute msg)
    , trayNode : String
    }


{-| `Toast.Config` is something that holds information about
rendering stuff.
-}
type alias Config msg =
    Private (Config_ msg)


{-| To create an empty [Toast.Config](#Config) you have to provide
a `Toast.Msg -> msg` function, this probably should look like

    type Msg
        = ToastMsg Toast.Msg
          -- other stuff...
        | AddToast MyToastContent

    toastConfig : Toast.Config Msg
    toastConfig =
        Toast.config ToastMsg

-}
config : (Msg -> msg) -> Config msg
config toAppMsg =
    Private
        { toAppMsg = toAppMsg
        , enterAttributes = []
        , exitAttributes = []
        , focusAttributes = []
        , toastAttributes = []
        , trayAttributes = []
        , trayNode = "div"
        }


{-| Add custom attributes to toasts only during their
[Toast.Enter](#Phase) phase.

    import Html.Attributes exposing (style)

    Toast.config ToastMsg
        |> Toast.withEnterAttributes [ style "opacity" "0" ]
        |> Toast.render viewToast model.tray

-}
withEnterAttributes :
    List (Html.Attribute msg)
    -> Config msg
    -> Config msg
withEnterAttributes attrs (Private cfg) =
    Private { cfg | enterAttributes = cfg.enterAttributes ++ attrs }


{-| Add custom attributes to toasts only during their
[Toast.Exit](#Phase) phase.

    Toast.config ToastMsg
        |> Toast.withExitAttributes [ style "transform" "translateX(20em)" ]
        |> Toast.render viewToast model.tray

-}
withExitAttributes :
    List (Html.Attribute msg)
    -> Config msg
    -> Config msg
withExitAttributes attrs (Private cfg) =
    Private { cfg | exitAttributes = cfg.exitAttributes ++ attrs }


{-| Add custom attributes to toasts when they're [focused](#Interaction).

    Toast.config ToastMsg
        |> Toast.withFocusAttributes [ style "box-shadow" "2px 3px 7px 2px #c8cdd0" ]
        |> Toast.render viewToast model.tray

-}
withFocusAttributes :
    List (Html.Attribute msg)
    -> Config msg
    -> Config msg
withFocusAttributes attrs (Private cfg) =
    Private { cfg | focusAttributes = cfg.focusAttributes ++ attrs }


{-| Add custom attributes to every toasts.

    Toast.config ToastMsg
        |> Toast.withAttributes [ style "background" "#4a90e2" ]
        |> Toast.render viewToast model.tray

-}
withAttributes :
    List (Html.Attribute msg)
    -> Config msg
    -> Config msg
withAttributes attrs (Private cfg) =
    Private { cfg | toastAttributes = cfg.toastAttributes ++ attrs }


{-| Shortcut for [withEnterAttributes](#withEnterAttributes) plus [withExitAttributes](#withExitAttributes).

    Toast.config ToastMsg
        |> Toast.withEnterAttributes [ class "toast-fade-out" ]
        |> Toast.withExitAttributes [ class "toast-fade-out" ]
        |> Toast.render viewToast model.tray

    Toast.config ToastMsg
        |> Toast.withTransitionAttributes [ class "toast-fade-out" ]
        |> Toast.render viewToast model.tray

-}
withTransitionAttributes :
    List (Html.Attribute msg)
    -> Config msg
    -> Config msg
withTransitionAttributes attrs (Private cfg) =
    Private
        { cfg
            | enterAttributes = cfg.enterAttributes ++ attrs
            , exitAttributes = cfg.exitAttributes ++ attrs
        }


{-| Add custom attributes to rendered [Toast.Tray](#Tray).

    Toast.config ToastMsg
        |> Toast.withTrayAttributes [ class "nice-tray" ]
        |> Toast.render viewToast model.tray

-}
withTrayAttributes :
    List (Html.Attribute msg)
    -> Config msg
    -> Config msg
withTrayAttributes attrs (Private cfg) =
    Private { cfg | trayAttributes = cfg.trayAttributes ++ attrs }


{-| Set [nodeName](https://developer.mozilla.org/en-US/docs/Web/API/Node/nodeName)
of rendered [Toast.Tray](#Tray).

By default this is `"div"`. I know, as a `String`,
but hey that's what you get from [Html.Keyed.node](https://package.elm-lang.org/packages/elm/html/latest/Html-Keyed#node)

-}
withTrayNode :
    String
    -> Config msg
    -> Config msg
withTrayNode node (Private cfg) =
    Private { cfg | trayNode = node }


mapAttributes :
    (Msg -> msg)
    -> List (Html.Attribute Msg)
    -> List (Html.Attribute msg)
mapAttributes mapper =
    List.map (Html.Attributes.map mapper)


defaultAttributes : (Msg -> msg) -> Id -> List (Html.Attribute msg)
defaultAttributes toAppMsg id =
    mapAttributes toAppMsg
        [ Html.Attributes.tabindex 0
        , Html.Events.onMouseEnter (Interaction Focus id)
        , Html.Events.onMouseLeave (Interaction Blur id)
        , Html.Events.onFocus (Interaction Focus id)
        , Html.Events.onBlur (Interaction Blur id)
        ]


transitionAttributes : Config_ msg -> Phase -> List (Html.Attribute msg)
transitionAttributes cfg phase =
    case phase of
        Enter ->
            cfg.enterAttributes

        In ->
            []

        Exit ->
            cfg.exitAttributes


interactionAttributes : Config_ msg -> Interaction -> List (Html.Attribute msg)
interactionAttributes cfg interaction =
    case interaction of
        Focus ->
            cfg.focusAttributes

        Blur ->
            []


allToastAttributes : Config_ msg -> Toast_ content -> List (Html.Attribute msg)
allToastAttributes cfg toast =
    List.concat
        [ cfg.toastAttributes
        , interactionAttributes cfg toast.interaction
        , transitionAttributes cfg toast.phase
        , defaultAttributes cfg.toAppMsg toast.id
        ]


{-| `Toast.Info` represent data publicly exposed about a toast.

You already know [Toast.Phase](#Phase) and [Toast.Interaction](#Interaction),
of course you also know `content` since this is your own data.

Meet `id`, this little field contains a unique value for each toast
that you need to pass to [Toast.remove](#remove) and [Toast.exit](#exit).

-}
type alias Info content =
    { id : Private Id
    , phase : Phase
    , interaction : Interaction
    , content : content
    }


toInfo : Toast_ content -> Info content
toInfo toast =
    { id = Private toast.id
    , phase = toast.phase
    , interaction = toast.interaction
    , content = toast.content
    }


renderToast :
    (List (Html.Attribute msg) -> Info content -> Html msg)
    -> Config_ msg
    -> Toast_ content
    -> ( String, Html msg )
renderToast viewer cfg toast =
    ( "toast-" ++ String.fromInt toast.id
    , viewer (allToastAttributes cfg toast) (toInfo toast)
    )


{-| This function is where our money are: all our data shrunk down
to a beautiful `Html msg`, ready to be served.

The first thing needed to make this magic is a `viewToast` function
that I'll try to explain how it works:

  - It takes all the html attributes compiled by `elm-toast`
    which you need to remember to attach to some node
  - Takes a [Toast.Info](#Info) so you can access to your
    toast's `content` and other stuff
  - Return an `Html msg` that represent your toast,
    or something that contains your toast like a wrapper
    or whatever

Secondly you have to provide a [Toast.Tray](#Tray)
and last but not least your [Toast.Config](#Config).

    viewToast : List (Html.Attribute Msg) -> Toast.Info Toast -> Html Msg
    viewToast attrs toast =
        Html.div
            attrs -- do not forget this little friend!
            [ Html.text toast.content.message ]

    Toast.render viewToast model.tray toastConfig

-}
render :
    (List (Html.Attribute msg) -> Info content -> Html msg)
    -> Tray content
    -> Config msg
    -> Html msg
render viewer (Private model) (Private cfg) =
    model.toasts
        |> List.map (renderToast viewer cfg)
        |> Html.Keyed.node cfg.trayNode cfg.trayAttributes


{-| Inside your [viewToast](#render) you may want to remove
your toast programmatically.
Remove means that the toast is deleted right away.
If you want to go through the exit transition use [exit](#exit).

    closeButton : Toast.Info Toast -> Html Msg
    closeButton toast =
        Html.div
            [ onClick <| ToastMsg (Toast.exit toast.id) ]
            [ Html.text "✘" ]

    viewToast : List (Html.Attribute Msg) -> Toast.Info Toast -> Html Msg
    viewToast attrs toast =
        Html.div
            attrs
            [ Html.text toast.content.message
            , closeButton toast
            ]

-}
remove : Private Id -> Msg
remove (Private id) =
    Remove id


{-| Same as [remove](#remove), but the toast goes through
its exit transition phase.
If you have a fade-out animation it'll be showed.
-}
exit : Private Id -> Msg
exit (Private id) =
    Transition Exit id
