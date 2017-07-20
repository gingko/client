port module ListWindow exposing (..)


import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Sha1 exposing (sha1)


main : Program (List (String, String)) Model Msg
main =
  programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


port openTree : (String, String) -> Cmd msg




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
    UpdateNewField str ->
      { model
        | field = str
      }
        ! []

    New ->
      { model
        | field = ""
      }
        ! [ openTree (Sha1.sha1 model.field, model.field) ]

    Load (id, name) ->
      { model
        | field = ""
      }
        ! [ openTree (id, name) ]

    _ ->
      model ! []




-- VIEW

view : Model -> Html Msg
view model =
  ul []
    (
      [ li []
        [ input [ onInput UpdateNewField ][]
        , button [ onClick New ][text "New"]
        ]
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
