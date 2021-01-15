module Upgrade exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, option, select, text)
import Html.Attributes exposing (id, selected, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onChange)
import Json.Encode as Enc
import SharedUI exposing (modalWrapper)



-- MODEL


type alias Model =
    { currency : CurrencySelection
    , billing : BillingFrequency
    , plan : Plan
    }


type BillingFrequency
    = Monthly
    | Yearly


type Plan
    = Regular
    | Discount
    | Bonus


planToString : Plan -> String
planToString plan =
    case plan of
        Regular ->
            "regular"

        Discount ->
            "discount"

        Bonus ->
            "bonus"


init : Model
init =
    { currency = UnknownCurrency
    , billing = Monthly
    , plan = Regular
    }


toValue : Model -> Enc.Value
toValue model =
    case model.currency of
        UnknownCurrency ->
            Enc.null

        Currency curr ->
            Enc.object
                [ ( "currency", Enc.string (currencyToString curr) )
                , ( "billing"
                  , if model.billing == Monthly then
                        "monthly" |> Enc.string

                    else
                        "yearly" |> Enc.string
                  )
                , ( "plan", Enc.string (planToString model.plan) )
                ]



-- UPDATE


type Msg
    = CurrencySelected String
    | CheckoutClicked Enc.Value
    | UpgradeModalClosed


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
        |> modalWrapper UpgradeModalClosed (Just "upgrade-modal") "Upgrade Gingko Writer"


viewCopy : Html msg
viewCopy =
    div [ id "upgrade-copy" ] [ text "body copy here" ]


viewPaymentForm : Model -> Html Msg
viewPaymentForm model =
    case model.currency of
        UnknownCurrency ->
            div [ id "upgrade-checkout" ]
                [ viewCurrencySelector CurrencySelected model
                ]

        Currency curr ->
            div [ id "upgrade-checkout" ]
                [ viewCurrencySelector CurrencySelected model
                , text "Price is $10/mo"
                , button [ onClick <| CheckoutClicked (toValue model) ] [ text "Pay Now" ]
                ]


viewCurrencySelector : (String -> Msg) -> Model -> Html Msg
viewCurrencySelector selectMsg model =
    select [ id "currency-selector", onChange selectMsg ]
        [ option [ value "" ] [ text "Select your currency" ]
        , option [ value "USD", selected (model.currency == Currency USD) ] [ text "USD" ]
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
        "USD" ->
            Just USD

        _ ->
            Nothing
