module Upgrade exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, br, button, div, hr, input, label, option, p, select, small, span, text)
import Html.Attributes exposing (checked, class, for, id, name, selected, type_, value)
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


activeCurrencies =
    [ USD, EUR, INR, GBP, CAD, CNY ]


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
    | PlanChanged Plan
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

        PlanChanged plan ->
            { model | plan = plan }

        _ ->
            model



-- VIEW


view : Model -> List (Html Msg)
view model =
    [ viewCopy model
    , viewPaymentForm model
    ]
        |> modalWrapper UpgradeModalClosed (Just "upgrade-modal") "Upgrade Gingko Writer"


viewCopy : Model -> Html Msg
viewCopy model =
    div [ id "upgrade-copy" ]
        [ p [] [ text "Gingko has helped people shave years off their thesis, helped bestselling writers finish their novels, and reduced overwhelm for thousands." ]
        , p [] [ text "If you've found the free trial useful, you can upgrade to the full version." ]
        , p [] [ text "With gratitude,", br [] [], text "Adriano Ferrari" ]
        , hr [] []
        , input
            [ id "plan-discount"
            , type_ "radio"
            , name "plan"
            , checked (model.plan == Discount)
            , onInput (always (PlanChanged Discount))
            ]
            []
        , label [ for "plan-discount" ] [ text "Discount" ]
        , input
            [ id "plan-regular"
            , type_ "radio"
            , name "plan"
            , checked (model.plan == Regular)
            , onInput (always (PlanChanged Regular))
            ]
            []
        , label [ for "plan-regular" ] [ text "Regular" ]
        , input
            [ id "plan-bonus"
            , type_ "radio"
            , name "plan"
            , checked (model.plan == Bonus)
            , onInput (always (PlanChanged Bonus))
            ]
            []
        , label [ for "plan-bonus" ] [ text "Bonus" ]
        ]


viewPaymentForm : Model -> Html Msg
viewPaymentForm model =
    case model.currency of
        UnknownCurrency ->
            div [ id "upgrade-checkout" ]
                [ viewCurrencySelector CurrencySelected model
                ]

        Currency curr ->
            let
                currSymbol =
                    case curr of
                        CNY ->
                            "元"

                        CAD ->
                            Money.toSymbol curr

                        _ ->
                            Money.toNativeSymbol curr

                symbolRight =
                    case curr of
                        CNY ->
                            True

                        _ ->
                            False
            in
            div [ id "upgrade-checkout" ]
                [ viewCurrencySelector CurrencySelected model
                , br [] []
                , input
                    [ id "monthly"
                    , type_ "radio"
                    , name "billing"
                    , checked (model.billing == Monthly)
                    , onInput (always (BillingChanged Monthly))
                    ]
                    []
                , label [ for "monthly" ] [ text "Monthly" ]
                , br [] []
                , input
                    [ id "yearly"
                    , type_ "radio"
                    , name "billing"
                    , checked (model.billing == Yearly)
                    , onInput (always (BillingChanged Yearly))
                    ]
                    []
                , label [ for "yearly" ] [ text "Yearly (2 months free)" ]
                , hr [] []
                , div [ id "price-display" ]
                    [ div [ id "price-amount" ]
                        ([ small [] [ text currSymbol ]
                         , text (priceAmount curr model.billing model.plan)
                         ]
                            |> (if symbolRight then
                                    List.reverse

                                else
                                    identity
                               )
                        )
                    , small [] [ text (billingToString model.billing) ]
                    ]
                , hr [] []
                , button [ class "payment-button", onClick <| CheckoutClicked (toValue model) ] [ text "Checkout" ]
                ]


viewCurrencySelector : (String -> Msg) -> Model -> Html Msg
viewCurrencySelector selectMsg model =
    let
        unknownOption =
            if model.currency == UnknownCurrency then
                [ option [ value "" ] [ text "Select your currency" ] ]

            else
                []

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
        ( USD, Monthly, Discount ) ->
            "5"

        ( USD, Monthly, Regular ) ->
            "10"

        ( USD, Monthly, Bonus ) ->
            "15"

        ( USD, Yearly, Discount ) ->
            "49"

        ( USD, Yearly, Regular ) ->
            "99"

        ( USD, Yearly, Bonus ) ->
            "149"

        ( EUR, Monthly, Discount ) ->
            "3.40"

        ( EUR, Monthly, Regular ) ->
            "6.80"

        ( EUR, Monthly, Bonus ) ->
            "10.20"

        ( EUR, Yearly, Discount ) ->
            "34"

        ( EUR, Yearly, Regular ) ->
            "68"

        ( EUR, Yearly, Bonus ) ->
            "100"

        ( INR, Monthly, Discount ) ->
            "100"

        ( INR, Monthly, Regular ) ->
            "200"

        ( INR, Monthly, Bonus ) ->
            "300"

        ( INR, Yearly, Discount ) ->
            "1000"

        ( INR, Yearly, Regular ) ->
            "2000"

        ( INR, Yearly, Bonus ) ->
            "3000"

        ( GBP, Monthly, Discount ) ->
            "3.30"

        ( GBP, Monthly, Regular ) ->
            "6.60"

        ( GBP, Monthly, Bonus ) ->
            "9.90"

        ( GBP, Yearly, Discount ) ->
            "33"

        ( GBP, Yearly, Regular ) ->
            "66"

        ( GBP, Yearly, Bonus ) ->
            "99"

        ( CAD, Monthly, Discount ) ->
            "6"

        ( CAD, Monthly, Regular ) ->
            "12"

        ( CAD, Monthly, Bonus ) ->
            "18"

        ( CAD, Yearly, Discount ) ->
            "60"

        ( CAD, Yearly, Regular ) ->
            "119"

        ( CAD, Yearly, Bonus ) ->
            "179"

        ( CNY, Monthly, Discount ) ->
            "21"

        ( CNY, Monthly, Regular ) ->
            "42"

        ( CNY, Monthly, Bonus ) ->
            "63"

        ( CNY, Yearly, Discount ) ->
            "210"

        ( CNY, Yearly, Regular ) ->
            "420"

        ( CNY, Yearly, Bonus ) ->
            "630"

        _ ->
            "unset"


billingToString : BillingFrequency -> String
billingToString billing =
    case billing of
        Monthly ->
            "per month"

        Yearly ->
            "per year"
