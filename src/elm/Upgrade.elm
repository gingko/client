module Upgrade exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, option, select, text)
import Html.Attributes exposing (id, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onChange)
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
    = CurrencySelected String
    | CheckoutClicked
    | ModalClosed


update : Msg -> Model -> Model
update msg model =
    case msg of
        CurrencySelected currencyString ->
            case currencyFromString currencyString of
                Just currency ->
                    { model | currency = Currency currency }

                Nothing ->
                    model

        _ ->
            model



-- VIEW


view : Model -> List (Html Msg)
view model =
    [ viewCopy
    , viewPaymentForm model
    ]
        |> modalWrapper ModalClosed (Just "upgrade-modal") "Upgrade Gingko Writer"


viewCopy : Html msg
viewCopy =
    div [ id "upgrade-copy" ] [ text "body copy here" ]


viewPaymentForm : Model -> Html Msg
viewPaymentForm model =
    case model.currency of
        UnknownCurrency ->
            div [ id "upgrade-checkout" ]
                [ viewCurrencySelector CurrencySelected
                ]

        Currency curr ->
            div [ id "upgrade-checkout" ]
                [ viewCurrencySelector CurrencySelected
                , text "Price is $10/mo"
                , button [ onClick CheckoutClicked ] [ text "Pay Now" ]
                ]


viewCurrencySelector : (String -> Msg) -> Html Msg
viewCurrencySelector selectMsg =
    select [ id "currency-selector", onChange selectMsg ]
        [ option [ value "" ] [ text "Select your currency" ]
        , option [ value "usd" ] [ text "USD" ]
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


currencyFromString : String -> Maybe Currency
currencyFromString currencyString =
    case currencyString of
        "usd" ->
            Just USD

        _ ->
            Nothing
