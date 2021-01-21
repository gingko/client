module Page.Message exposing (..)

import Html exposing (Html, a, br, div, h1, p, text)
import Html.Attributes exposing (class, href, id)
import Route


viewSuccess : { title : String, body : List (Html msg) }
viewSuccess =
    { title = "Payment Successful"
    , body =
        [ div [ id "message-page" ]
            [ div [ class "message-card" ]
                [ h1 [] [ text "Thank you for your payment" ]
                , p [] [ text "Your support is greatly appreciated 😄." ]
                , br [] []
                , a [ href (Route.toString Route.Root), class "message-cta" ] [ text "Continue" ]
                ]
            ]
        ]
    }
