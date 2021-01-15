module Upgrade exposing (Model, init, view)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import SharedUI exposing (modalWrapper)



-- MODEL


type alias Model =
    { currency : CurrencySelection
    , billing : BillingFrequency
    , pwywSelection : PwywSelection
    }


type CurrencySelection
    = UnknownCurrency
    | Currency Currency


type Currency
    = USD


type BillingFrequency
    = Monthly
    | Yearly


type PwywSelection
    = Regular
    | Discount
    | Bonus


init : Model
init =
    { currency = UnknownCurrency
    , billing = Monthly
    , pwywSelection = Regular
    }



-- UPDATE
-- VIEW


view : { modalClosedMsg : msg, checkoutClickedMsg : msg } -> List (Html msg)
view { modalClosedMsg, checkoutClickedMsg } =
    [ viewCopy
    , viewPaymentForm checkoutClickedMsg
    ]
        |> modalWrapper modalClosedMsg (Just "upgrade-modal") "Upgrade Gingko Writer"


viewCopy : Html msg
viewCopy =
    div [ id "upgrade-copy" ] [ text "body copy here" ]


viewPaymentForm : msg -> Html msg
viewPaymentForm checkoutClickedMsg =
    div [ id "upgrade-checkout" ]
        [ text "pricing and checkout here"
        , button [ onClick checkoutClickedMsg ] [ text "Pay Now" ]
        ]
