module Upgrade exposing (Model, Msg(..), init, toValue, update, view)

import Ant.Icons.Svg as Icon
import Html exposing (Html, a, br, button, div, h3, hr, img, input, label, li, option, p, select, small, span, strong, text, textarea, ul)
import Html.Attributes exposing (checked, class, classList, for, height, href, id, name, placeholder, selected, src, style, target, type_, value, width)
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
    , pwywOpen : Bool
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


planToString : Plan -> String
planToString plan =
    case plan of
        Regular ->
            "regular"


init : Model
init =
    { currency = Currency USD
    , billing = Monthly
    , plan = Regular
    , pwywOpen = False
    }


toValue : String -> Model -> Enc.Value
toValue eml model =
    case model.currency of
        UnknownCurrency ->
            Enc.null

        Currency curr ->
            Enc.object
                [ ( "email", Enc.string eml )
                , ( "currency", Enc.string (Money.toString curr) )
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
    | PWYWToggled Bool
    | CheckoutClicked Model
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

        PWYWToggled isOpen ->
            { model | pwywOpen = isOpen }

        _ ->
            model



-- VIEW


view : Maybe Int -> Model -> List (Html Msg)
view daysLeft_ model =
    [ viewTrialInfo daysLeft_
    , viewCopy
    , viewPaymentForm model
    ]
        |> modalWrapper UpgradeModalClosed (Just "upgrade-modal") Nothing "Upgrade Gingko Writer"


viewTrialInfo : Maybe Int -> Html Msg
viewTrialInfo daysLeft_ =
    case daysLeft_ of
        Just daysLeft ->
            if daysLeft <= 7 && daysLeft >= 0 then
                text ""

            else if daysLeft > 7 && daysLeft >= 0 then
                div [ id "upgrade-trial-info" ] [ text <| "You have " ++ String.fromInt daysLeft ++ " days left in your free trial." ]

            else
                div [ id "upgrade-trial-info" ] [ text "Your free trial has expired." ]

        Nothing ->
            text ""


viewCopy : Html Msg
viewCopy =
    div [ id "upgrade-copy" ]
        [ p [] [ text "Gingko Writer has helped people shave years off their thesis, helped bestselling writers finish their novels, and reduced overwhelm for thousands." ]
        , p [] [ text "If you've found the free trial useful, you can upgrade to the paid version." ]
        , p [] [ text "With gratitude,", br [] [], text "Adriano Ferrari" ]
        , img [ src "adriano-small-circle.jpg", width 82 ] []
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

                amountString =
                    priceAmount curr model.billing model.plan

                yearlySaving =
                    savingAmount curr
            in
            div [ id "upgrade-checkout" ]
                [ viewCurrencySelector CurrencySelected model
                , hr [] []
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
                , label [ for "yearly" ] [ text <| "Yearly (Save " ++ yearlySaving ++ ")" ]
                , hr [] []
                , div [ id "price-display" ]
                    [ div [ id "price-amount" ]
                        ([ small [] [ text currSymbol ]
                         , text amountString
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
                , button [ class "payment-button", onClick <| CheckoutClicked model ] [ text "Checkout" ]
                , small [ id "stripe-climate" ] [ img [ src "stripe-climate-badge.svg", width 14, style "margin-bottom" "-3px", style "margin-right" "5px" ] [], a [ href "https://climate.stripe.com/yqdXvm", target "_blank", class "climate-link" ] [ text "3% of your purchase" ], text " goes to removing CO₂ from the atmosphere." ]
                ]


viewCurrencySelector : (String -> Msg) -> Model -> Html Msg
viewCurrencySelector selectMsg model =
    let
        isUnknown =
            model.currency == UnknownCurrency

        unknownOption =
            if isUnknown then
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
    select [ id "currency-selector", classList [ ( "unknown-currency", isUnknown ) ], onChange selectMsg ]
        (unknownOption
            ++ (activeCurrencies |> List.map currencyOption)
        )



-- PLAN AMOUNTS


priceAmount : Currency -> BillingFrequency -> Plan -> String
priceAmount currency freq plan =
    case ( currency, freq, plan ) of
        ( USD, Monthly, Regular ) ->
            "12.75"

        ( USD, Yearly, Regular ) ->
            "117"

        ( EUR, Monthly, Regular ) ->
            "8.66"

        ( EUR, Yearly, Regular ) ->
            "80"

        ( INR, Monthly, Regular ) ->
            "255"

        ( INR, Yearly, Regular ) ->
            "2400"

        ( GBP, Monthly, Regular ) ->
            "8.40"

        ( GBP, Yearly, Regular ) ->
            "78"

        ( CAD, Monthly, Regular ) ->
            "12.75"

        ( CAD, Yearly, Regular ) ->
            "117"

        ( CNY, Monthly, Regular ) ->
            "55.55"

        ( CNY, Yearly, Regular ) ->
            "500"

        _ ->
            "unset"


savingAmount curr =
    case curr of
        USD ->
            "24%"

        EUR ->
            "23%"

        INR ->
            "22%"

        GBP ->
            "23%"

        CAD ->
            "24%"

        CNY ->
            "25%"

        _ ->
            "unset"


billingToString : BillingFrequency -> String
billingToString billing =
    case billing of
        Monthly ->
            "per month"

        Yearly ->
            "per year"
