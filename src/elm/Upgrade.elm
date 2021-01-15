module Upgrade exposing (Model, init, view)

import Html exposing (Html, button, div, option, select, text)
import Html.Attributes exposing (id, value)
import Html.Events exposing (onClick, onInput)
import SharedUI exposing (modalWrapper)



-- MODEL


type alias Model =
    { currency : CurrencySelection
    , billing : BillingFrequency
    , pwywSelection : PwywSelection
    }


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


type Msg
    = CurrencySelected Currency


update : Msg -> Model -> Model
update msg model =
    case msg of
        CurrencySelected currency ->
            { model | currency = Currency currency }



-- VIEW


view : { modalClosedMsg : msg, checkoutClickedMsg : msg } -> Model -> List (Html msg)
view { modalClosedMsg, checkoutClickedMsg } model =
    [ viewCopy
    , viewPaymentForm checkoutClickedMsg model
    ]
        |> modalWrapper modalClosedMsg (Just "upgrade-modal") "Upgrade Gingko Writer"


viewCopy : Html msg
viewCopy =
    div [ id "upgrade-copy" ] [ text "body copy here" ]


viewPaymentForm : msg -> Model -> Html msg
viewPaymentForm checkoutClickedMsg model =
    case model.currency of
        UnknownCurrency ->
            div [ id "upgrade-checkout" ]
                [ select [ id "currency-selector" ]
                    [ option [ value "" ] [ text "Select your currency" ]
                    , option [ value "usd" ] [ text "USD" ]
                    ]
                ]

        Currency curr ->
            div [ id "upgrade-checkout" ]
                [ text "pricing and checkout here"
                , button [ onClick checkoutClickedMsg ] [ text "Pay Now" ]
                ]



-- CURRENCIES


type CurrencySelection
    = UnknownCurrency
    | Currency Currency


type Currency
    = USD


currencyToString : Currency -> String
currencyToString currency =
    case currency of
        USD ->
            "USD"
