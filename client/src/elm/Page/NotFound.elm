module Page.NotFound exposing (view)

import Html exposing (Html, div, text)


view : { title : String, body : List (Html msg) }
view =
    { title = "Not Found"
    , body = [ div [] [ text "Not Found" ] ]
    }
