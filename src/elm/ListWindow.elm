port module ListWindow exposing (..)


import Html exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program (List (String, String)) Model Msg
main =
  programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }




-- MODEL


type alias Model =
  { trees : List (String, String)
  , field : String
  }


defaultModel : Model
defaultModel =
  { trees = []
  , field = ""
  }


init : List (String, String) -> (Model, Cmd Msg)
init listIn =
  { defaultModel
    | trees = listIn
  }
    ! []




-- UPDATE

type Msg
  = NoOp
  | UpdateNewField String
  | New
  | Load (String, String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    _ ->
      model ! []




-- VIEW

view : Model -> Html Msg
view model =
  ul []
    ([ input [][]
    ]
    ++ List.map viewTreeItem model.trees
    )


viewTreeItem : (String, String) -> Html Msg
viewTreeItem (id, name) =
  li [onClick (Load (id, name))]
    [ text name ]




-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [
    ]
