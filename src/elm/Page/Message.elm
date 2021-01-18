module Page.Message exposing (..)

import Html exposing (Html, div, text)


view : String -> String -> { title : String, body : List (Html msg) }
view titleString bodyString =
    { title = titleString
    , body = [ div [] [ text bodyString ] ]
    }
