module UI.Sidebar exposing (SidebarMenuState(..), SidebarState(..), viewSidebar, viewSidebarStatic)

import Ant.Icons.Svg as AntIcons
import Browser.Dom exposing (Element)
import Doc.List as DocList exposing (Model(..))
import GlobalData exposing (GlobalData)
import Html exposing (Html, a, br, button, div, h2, hr, img, input)
import Html.Attributes exposing (action, class, classList, href, id, method, name, src, style, target, type_, value, width)
import Html.Attributes.Extra exposing (attributeIf)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Extra exposing (viewIf)
import MD5
import Session exposing (PaymentStatus(..), Session)
import Translation exposing (Language(..), TranslationId(..), langToString, languageName)
import Types exposing (SortBy, TooltipPosition(..))
import Utils exposing (emptyText, onClickStop, text, textNoTr)


type SidebarState
    = SidebarClosed
    | File


type SidebarMenuState
    = NoSidebarMenu
    | Account (Maybe Element)


type alias SidebarMsgs msg =
    { sidebarStateChanged : SidebarState -> msg
    , noOp : msg
    , clickedNew : msg
    , tooltipRequested : String -> TooltipPosition -> TranslationId -> msg
    , tooltipClosed : msg
    , clickedSwitcher : msg
    , clickedHelp : msg
    , clickedEmailSupport : msg
    , clickedShowVideos : msg
    , languageMenuRequested : Maybe String -> msg
    , toggledAccount : Bool -> msg
    , upgrade : msg
    , logout : msg
    , fileSearchChanged : String -> msg
    , changeSortBy : SortBy -> msg
    , contextMenuOpened : String -> ( Float, Float ) -> msg
    , languageChanged : Language -> msg
    , fullscreenRequested : msg
    }


viewSidebar :
    GlobalData
    -> Session
    -> SidebarMsgs msg
    -> String
    -> SortBy
    -> String
    -> DocList.Model
    -> String
    -> Maybe String
    -> SidebarMenuState
    -> SidebarState
    -> Html msg
viewSidebar globalData session msgs currentDocId sortCriteria fileFilter docList accountEmail contextTarget_ dropdownState sidebarState =
    let
        lang =
            GlobalData.language globalData

        custId_ =
            case Session.paymentStatus (GlobalData.currentTime globalData) session of
                Customer custId ->
                    Just custId

                _ ->
                    Nothing

        isOpen =
            not (sidebarState == SidebarClosed)

        accountOpen =
            case dropdownState of
                Account _ ->
                    True

                _ ->
                    False

        toggle menu =
            if sidebarState == menu then
                msgs.sidebarStateChanged <| SidebarClosed

            else
                msgs.sidebarStateChanged <| menu

        viewIf cond v =
            if cond then
                v

            else
                emptyText
    in
    div [ id "sidebar", onClick <| toggle File, classList [ ( "open", isOpen ) ] ]
        ([ div [ id "brand" ]
            ([ img [ src "../gingko-leaf-logo.svg", width 28 ] [] ]
                ++ (if isOpen then
                        [ h2 [ id "brand-name" ] [ Html.text "Gingko Writer" ]
                        , div [ id "sidebar-collapse-icon" ] [ AntIcons.leftOutlined [] ]
                        ]

                    else
                        [ emptyText ]
                   )
                ++ [ div [ id "hamburger-icon" ] [ AntIcons.menuOutlined [] ] ]
            )
         , div
            [ id "new-icon"
            , class "sidebar-button"
            , onClickStop msgs.clickedNew
            , onMouseEnter <| msgs.tooltipRequested "new-icon" RightTooltip NewDocument
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.fileAddOutlined [] ]
         , div
            [ id "documents-icon"
            , class "sidebar-button"
            , classList [ ( "open", isOpen ) ]
            , attributeIf (not isOpen) <| onMouseEnter <| msgs.tooltipRequested "documents-icon" RightTooltip ShowDocumentList
            , attributeIf (not isOpen) <| onMouseLeave msgs.tooltipClosed
            ]
            [ if isOpen then
                AntIcons.folderOpenOutlined []

              else
                AntIcons.folderOutlined []
            ]
         , viewIf isOpen <|
            DocList.viewSidebarList
                { noOp = msgs.noOp
                , filter = msgs.fileSearchChanged
                , changeSortBy = msgs.changeSortBy
                , contextMenu = msgs.contextMenuOpened
                , tooltipRequested = msgs.tooltipRequested
                , tooltipClosed = msgs.tooltipClosed
                }
                currentDocId
                sortCriteria
                contextTarget_
                fileFilter
                docList
         , div
            [ id "document-switcher-icon"
            , onClickStop msgs.clickedSwitcher
            , onMouseEnter <| msgs.tooltipRequested "document-switcher-icon" RightTooltip OpenQuickSwitcher
            , onMouseLeave msgs.tooltipClosed
            , class "sidebar-button"
            , attributeIf (docList == Success []) (class "disabled")
            ]
            [ AntIcons.fileSearchOutlined [] ]
         , div
            [ id "help-icon"
            , class "sidebar-button"
            , onClickStop msgs.clickedHelp
            , onMouseEnter <| msgs.tooltipRequested "help-icon" RightTooltip Help
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.questionCircleFilled [] ]
         , div
            [ id "notifications-icon"
            , class "sidebar-button"
            , onClickStop <| msgs.noOp
            , onMouseEnter <| msgs.tooltipRequested "notifications-icon" RightTooltip WhatsNew
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.bellOutlined [] ]
         , div
            [ id "account-icon"
            , class "sidebar-button"
            , classList [ ( "open", accountOpen ) ]
            , onClickStop <| msgs.toggledAccount (not accountOpen)
            , attributeIf (not accountOpen) <| onMouseEnter <| msgs.tooltipRequested "account-icon" RightTooltip AccountTooltip
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.userOutlined [] ]
         ]
            ++ viewSidebarMenu lang
                custId_
                { clickedEmailSupport = msgs.clickedEmailSupport
                , clickedShowVideos = msgs.clickedShowVideos
                , helpClosed = msgs.clickedHelp
                , languageMenuRequested = msgs.languageMenuRequested
                , languageChanged = msgs.languageChanged
                , logout = msgs.logout
                , toggledAccount = msgs.toggledAccount
                , upgrade = msgs.upgrade
                , noOp = msgs.noOp
                }
                accountEmail
                dropdownState
        )


viewSidebarMenu :
    Language
    -> Maybe String
    ->
        { clickedEmailSupport : msg
        , clickedShowVideos : msg
        , helpClosed : msg
        , languageMenuRequested : Maybe String -> msg
        , languageChanged : Language -> msg
        , logout : msg
        , toggledAccount : Bool -> msg
        , upgrade : msg
        , noOp : msg
        }
    -> String
    -> SidebarMenuState
    -> List (Html msg)
viewSidebarMenu lang custId_ msgs accountEmail dropdownState =
    case dropdownState of
        Account langMenuEl_ ->
            let
                gravatarImg =
                    img
                        [ src ("https://www.gravatar.com/avatar/" ++ (accountEmail |> String.trim |> String.toLower |> MD5.hex) ++ "?d=mp")
                        , class "icon"
                        ]
                        []

                manageSubBtn =
                    case custId_ of
                        Just custId ->
                            Html.form [ method "POST", action "/create-portal-session" ]
                                [ input [ type_ "hidden", name "customer_id", value custId ] []
                                , button [ id "manage-subscription-button", type_ "submit" ]
                                    [ div [ class "icon" ] [ AntIcons.creditCardOutlined [] ]
                                    , text lang ManageSubscription
                                    ]
                                ]

                        Nothing ->
                            div
                                [ onClickStop msgs.upgrade
                                , class "sidebar-menu-item"
                                ]
                                [ div [ class "icon" ] [ AntIcons.creditCardOutlined [] ], text lang Upgrade ]
            in
            [ div [ id "account-menu", class "sidebar-menu" ]
                [ div [ onClickStop msgs.noOp, class "sidebar-menu-item", class "no-action" ]
                    [ gravatarImg, Html.text accountEmail ]
                , hr [] []
                , a [ href "{%TESTIMONIAL_URL%}", onClickStop msgs.noOp, target "_blank", class "sidebar-menu-item" ]
                    [ div [ class "icon" ] [ AntIcons.giftOutlined [] ]
                    , text lang WordOfMouthCTA1
                    , br [] []
                    , text lang WordOfMouthCTA2
                    ]
                , hr [] []
                , manageSubBtn
                , div
                    [ id "language-option"
                    , class "sidebar-menu-item"
                    , if langMenuEl_ == Nothing then
                        onClickStop <| msgs.languageMenuRequested (Just "language-option")

                      else
                        onClickStop <| msgs.languageMenuRequested Nothing
                    ]
                    [ div [ class "icon" ] [ AntIcons.globalOutlined [] ]
                    , textNoTr (languageName lang)
                    , div [ class "right-icon" ] [ AntIcons.rightOutlined [] ]
                    ]
                , hr [] []
                , div
                    [ id "logout-button", class "sidebar-menu-item", onClickStop msgs.logout ]
                    [ div [ class "icon" ] [ AntIcons.logoutOutlined [] ]
                    , text lang Logout
                    ]
                ]
            , case langMenuEl_ of
                Just langMenuEl ->
                    let
                        bottPx =
                            langMenuEl.scene.height - langMenuEl.element.y - langMenuEl.element.height - 8
                    in
                    div
                        [ id "language-menu"
                        , class "sidebar-menu"
                        , style "left" ((langMenuEl.element.x + langMenuEl.element.width |> String.fromFloat) ++ "px")
                        , style "bottom" ((bottPx |> String.fromFloat) ++ "px")
                        , style "max-height" ((langMenuEl.viewport.height - 41 - bottPx |> String.fromFloat) ++ "px")
                        ]
                        ((Translation.activeLanguages
                            |> List.map
                                (\( langOpt, langName ) ->
                                    div
                                        [ id <| "lang-" ++ langToString langOpt
                                        , onClickStop <| msgs.languageChanged langOpt
                                        , class "sidebar-menu-item"
                                        , classList [ ( "selected", langOpt == lang ) ]
                                        ]
                                        [ textNoTr langName ]
                                )
                         )
                            ++ [ a
                                    [ href "https://poeditor.com/join/project?hash=k8Br3k0JVz"
                                    , target "_blank"
                                    , class "sidebar-menu-item"
                                    , onClickStop <| msgs.toggledAccount False
                                    ]
                                    [ text lang ContributeTranslations ]
                               ]
                        )

                Nothing ->
                    emptyText
            , viewIf (langMenuEl_ == Nothing) <| div [ id "help-menu-exit-top", onMouseEnter <| msgs.toggledAccount False ] []
            , viewIf (langMenuEl_ == Nothing) <| div [ id "help-menu-exit-right", onMouseEnter <| msgs.toggledAccount False ] []
            ]

        NoSidebarMenu ->
            [ emptyText ]


viewSidebarStatic : Bool -> List (Html msg)
viewSidebarStatic sidebarOpen =
    [ div [ id "sidebar", classList [ ( "open", sidebarOpen ) ], class "static" ]
        [ div [ id "brand" ]
            ([ img [ src "../gingko-leaf-logo.svg", width 28 ] [] ]
                ++ (if sidebarOpen then
                        [ h2 [ id "brand-name" ] [ text En (NoTr "Gingko Writer") ]
                        , div [ id "sidebar-collapse-icon" ] [ AntIcons.leftOutlined [] ]
                        ]

                    else
                        [ emptyText ]
                   )
            )
        , viewIf sidebarOpen <| div [ id "sidebar-document-list-wrap" ] []
        , div [ id "new-icon", class "sidebar-button" ] [ AntIcons.fileAddOutlined [] ]
        , div [ id "documents-icon", class "sidebar-button", classList [ ( "open", sidebarOpen ) ] ]
            [ if sidebarOpen then
                AntIcons.folderOpenOutlined []

              else
                AntIcons.folderOutlined []
            ]
        , div [ id "document-switcher-icon", class "sidebar-button", class "disabled" ] [ AntIcons.fileSearchOutlined [] ]
        , div
            [ id "help-icon", class "sidebar-button" ]
            [ AntIcons.questionCircleFilled [] ]
        , div [ id "notifications-icon", class "sidebar-button" ] [ AntIcons.bellOutlined [] ]
        , div [ id "account-icon", class "sidebar-button" ] [ AntIcons.userOutlined [] ]
        ]
    ]
