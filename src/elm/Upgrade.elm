module Upgrade exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, br, button, div, hr, input, label, option, select, small, span, text)
import Html.Attributes exposing (checked, for, id, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onChange)
import Json.Encode as Enc
import Money exposing (Currency(..))
import SharedUI exposing (modalWrapper)



-- MODEL


type alias Model =
    { currency : CurrencySelection
    , billing : BillingFrequency
    , plan : Plan
    }


type CurrencySelection
    = UnknownCurrency
    | Currency Currency


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
                [ ( "currency", Enc.string (Money.toString curr) )
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
    | BillingChanged BillingFrequency
    | CheckoutClicked Enc.Value
    | UpgradeModalClosed


update : Msg -> Model -> Model
update msg model =
    case msg of
        CurrencySelected currencyString ->
            case Money.fromString currencyString of
                Just currency ->
                    { model | currency = Currency currency }

                Nothing ->
                    model

        BillingChanged billFreq ->
            { model | billing = billFreq }

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
                , br [] []
                , input
                    [ id "monthly"
                    , type_ "radio"
                    , checked (model.billing == Monthly)
                    , onInput (always (BillingChanged Monthly))
                    ]
                    []
                , label [ for "monthly" ] [ text "Monthly" ]
                , br [] []
                , input
                    [ id "yearly"
                    , type_ "radio"
                    , checked (model.billing == Yearly)
                    , onInput (always (BillingChanged Yearly))
                    ]
                    []
                , label [ for "yearly" ] [ text "Yearly" ]
                , hr [] []
                , div [ id "price-display" ]
                    [ div [ id "price-amount" ]
                        [ small [] [ text <| Money.toNativeSymbol curr ]
                        , text (priceAmount curr model.billing model.plan)
                        ]
                    , small [] [ text (billingToString model.billing) ]
                    ]
                , hr [] []
                , button [ onClick <| CheckoutClicked (toValue model) ] [ text "Pay Now" ]
                ]


viewCurrencySelector : (String -> Msg) -> Model -> Html Msg
viewCurrencySelector selectMsg model =
    let
        unknownOption =
            if model.currency == UnknownCurrency then
                [ option [ value "" ] [ text "Select your currency" ] ]

            else
                []

        activeCurrencies =
            [ USD, INR ]

        currencyOption curr =
            let
                currText =
                    Money.toString curr
            in
            option [ value currText, selected (model.currency == Currency curr) ]
                [ text <| currText ++ " - " ++ Money.toName { plural = True } curr ]
    in
    select [ id "currency-selector", onChange selectMsg ]
        (unknownOption
            ++ (activeCurrencies |> List.map currencyOption)
        )



-- PLAN AMOUNTS


priceAmount : Currency -> BillingFrequency -> Plan -> String
priceAmount currency freq plan =
    case ( currency, freq, plan ) of
        ( USD, Monthly, Regular ) ->
            "10"

        ( USD, Monthly, Discount ) ->
            "5"

        ( USD, Monthly, Bonus ) ->
            "15"

        ( USD, Yearly, Regular ) ->
            "99"

        ( USD, Yearly, Discount ) ->
            "49"

        ( USD, Yearly, Bonus ) ->
            "149"

        _ ->
            "unset"


billingToString : BillingFrequency -> String
billingToString billing =
    case billing of
        Monthly ->
            "per month"

        Yearly ->
            "per year"
