module UI.Sidebar exposing (SidebarMenuState(..), SidebarState(..), viewSidebar, viewSidebarStatic)

import Ant.Icons.Svg as AntIcons
import Browser.Dom exposing (Element)
import Css exposing (..)
import Doc.List as DocList exposing (Model(..))
import Feature
import Features exposing (Feature(..))
import GlobalData exposing (GlobalData)
import Html exposing (Html)
import Html.Styled exposing (a, button, div, form, fromUnstyled, h2, hr, img, input, text, toUnstyled)
import Html.Styled.Attributes as A exposing (action, class, classList, css, href, id, method, name, src, style, type_, value)
import Html.Styled.Events exposing (onClick, onMouseEnter, onMouseLeave)
import MD5
import Octicons
import Session exposing (LoggedIn, PaymentStatus(..))
import Translation exposing (Language(..), TranslationId(..), langToString, languageName)
import Types exposing (SortBy, TooltipPosition(..))
import Utils exposing (emptyText, onClickStopStyled, ternary, textElmCss)


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
    }


viewSidebar :
    GlobalData
    -> LoggedIn
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
            case Session.paymentStatus session of
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

        sidebarButtonCss =
            Css.batch
                [ width (px 40)
                , padding (px 10)
                , cursor pointer
                , property "fill" "var(--ui-1-fg)"
                , hover [ property "fill" "var(--ui-2-fg)" ]
                ]

        sidebarButtonOpen =
            Css.batch
                [ property "background" "var(--background-sidebar-menu)"
                , property "fill" "var(--ui-2-fg)"
                , borderRadius4 (px 5) (px 0) (px 0) (px 5)
                , paddingLeft (px 7)
                , marginLeft (px 3)
                , width (px 37)
                , property "box-shadow" "var(--small-shadow)"
                ]
    in
    div [ id "sidebar", onClick <| toggle File, classList [ ( "open", isOpen ) ] ]
        ([ div
            [ id "brand"
            , css
                []
            ]
            ([ img [ src "../gingko-leaf-logo.svg", A.width 28 ] [] ]
                ++ (if isOpen then
                        [ h2 [ id "brand-name" ] [ text "Gingko Writer" ]
                        , div [ id "sidebar-collapse-icon" ] [ AntIcons.leftOutlined [] |> fromUnstyled ]
                        ]

                    else
                        [ text "" ]
                   )
                ++ [ div [ id "hamburger-icon" ] [ AntIcons.menuOutlined [] |> fromUnstyled ] ]
            )
         , div
            [ id "new-icon"
            , css [ sidebarButtonCss, property "grid-area" "row1-icon" ]
            , onClickStopStyled msgs.clickedNew
            , onMouseEnter <| msgs.tooltipRequested "new-icon" RightTooltip NewDocument
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.fileAddOutlined [] |> fromUnstyled ]
         , div
            ([ id "documents-icon"
             , css
                ([ sidebarButtonCss, property "grid-area" "row2-icon" ]
                    ++ ternary isOpen
                        [ sidebarButtonOpen
                        , property "background" "var(--background-sidebar-menu)"
                        , marginLeft (px 4)
                        ]
                        []
                )
             ]
                ++ ternary (not isOpen)
                    [ onMouseEnter <| msgs.tooltipRequested "documents-icon" RightTooltip ShowDocumentList
                    , onMouseLeave msgs.tooltipClosed
                    ]
                    []
            )
            [ if isOpen then
                AntIcons.folderOpenOutlined [] |> fromUnstyled

              else
                AntIcons.folderOutlined [] |> fromUnstyled
            ]
         , viewIf isOpen
            (DocList.viewSidebarList
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
            )
            |> fromUnstyled
         , div
            ([ id "document-switcher-icon"
             , onClickStopStyled msgs.clickedSwitcher
             , onMouseEnter <| msgs.tooltipRequested "document-switcher-icon" RightTooltip OpenQuickSwitcher
             , onMouseLeave msgs.tooltipClosed
             , css [ sidebarButtonCss, property "grid-area" "row3-icon" ]
             ]
                ++ ternary (docList == DocList.Success []) [ class "disabled" ] []
            )
            [ AntIcons.fileSearchOutlined [] |> fromUnstyled ]
         , div
            [ id "help-icon"
            , css [ sidebarButtonCss, property "grid-area" "bott-row1" ]
            , onClickStopStyled msgs.clickedHelp
            , onMouseEnter <| msgs.tooltipRequested "help-icon" RightTooltip Help
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.questionCircleFilled [] |> fromUnstyled ]
         , div
            [ id "notifications-icon"
            , css [ sidebarButtonCss, property "grid-area" "bott-row2" ]
            , onClickStopStyled <| msgs.noOp
            , onMouseEnter <| msgs.tooltipRequested "notifications-icon" RightTooltip WhatsNew
            , onMouseLeave msgs.tooltipClosed
            ]
            [ AntIcons.bellOutlined [] |> fromUnstyled ]
         , div
            ([ id "account-icon"
             , css ([ sidebarButtonCss, property "grid-area" "bott-row3" ] ++ ternary accountOpen [ sidebarButtonOpen ] [])
             , onClickStopStyled <| msgs.toggledAccount (not accountOpen)
             , onMouseLeave msgs.tooltipClosed
             ]
                ++ ternary (not accountOpen) [ onMouseEnter <| msgs.tooltipRequested "account-icon" RightTooltip AccountTooltip ] []
            )
            [ AntIcons.userOutlined [] |> fromUnstyled ]
         ]
            ++ (viewSidebarMenu session
                    lang
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
                    |> List.map fromUnstyled
               )
        )
        |> toUnstyled


viewSidebarMenu :
    LoggedIn
    -> Language
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
viewSidebarMenu session lang custId_ msgs accountEmail dropdownState =
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
                            form [ method "POST", action "/create-portal-session" ]
                                [ input [ type_ "hidden", name "customer_id", value custId ] []
                                , button [ id "manage-subscription-button", type_ "submit" ]
                                    [ div [ class "icon" ] [ AntIcons.creditCardOutlined [] |> fromUnstyled ]
                                    , textElmCss lang ManageSubscription
                                    ]
                                ]

                        Nothing ->
                            div
                                [ onClickStopStyled msgs.upgrade
                                , class "sidebar-menu-item"
                                ]
                                [ div [ class "icon" ] [ AntIcons.creditCardOutlined [] |> fromUnstyled ], textElmCss lang Upgrade ]
            in
            [ div [ id "account-menu", class "sidebar-menu" ]
                [ div [ onClickStopStyled msgs.noOp, class "sidebar-menu-item", class "no-action" ]
                    [ gravatarImg, text accountEmail ]
                , hr [] []
                , ternary (Feature.enabled VotingAppLinkInMenu session)
                    (a [ href ("https://gingkowriter.voxemporium.com#" ++ Utils.voxEmporiumHash (Session.name session)), onClickStopStyled msgs.noOp, A.target "_blank", class "sidebar-menu-item" ]
                        [ div [ class "icon" ] [ Octicons.megaphone Octicons.defaultOptions |> fromUnstyled ]
                        , text "Vote on Improvements"
                        ]
                    )
                    (text "")
                , ternary (Feature.enabled VotingAppLinkInMenu session) (hr [] []) (text "")
                , manageSubBtn
                , div
                    [ id "language-option"
                    , class "sidebar-menu-item"
                    , if langMenuEl_ == Nothing then
                        onClickStopStyled <| msgs.languageMenuRequested (Just "language-option")

                      else
                        onClickStopStyled <| msgs.languageMenuRequested Nothing
                    ]
                    [ div [ class "icon" ] [ AntIcons.globalOutlined [] |> fromUnstyled ]
                    , text (languageName lang)
                    , div [ class "right-icon" ] [ AntIcons.rightOutlined [] |> fromUnstyled ]
                    ]
                , hr [] []
                , div
                    [ id "logout-button", class "sidebar-menu-item", onClickStopStyled msgs.logout ]
                    [ div [ class "icon" ] [ AntIcons.logoutOutlined [] |> fromUnstyled ]
                    , textElmCss lang Logout
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
                                        , onClickStopStyled <| msgs.languageChanged langOpt
                                        , class "sidebar-menu-item"
                                        , classList [ ( "selected", langOpt == lang ) ]
                                        ]
                                        [ text langName ]
                                )
                         )
                            ++ [ a
                                    [ href "https://poeditor.com/join/project?hash=k8Br3k0JVz"
                                    , A.target "_blank"
                                    , class "sidebar-menu-item"
                                    , onClickStopStyled <| msgs.toggledAccount False
                                    ]
                                    [ textElmCss lang ContributeTranslations ]
                               ]
                        )

                Nothing ->
                    text ""
            , ternary (langMenuEl_ == Nothing) (div [ id "help-menu-exit-top", onMouseEnter <| msgs.toggledAccount False ] []) (text "")
            , ternary (langMenuEl_ == Nothing) (div [ id "help-menu-exit-right", onMouseEnter <| msgs.toggledAccount False ] []) (text "")
            ]
                |> List.map toUnstyled

        NoSidebarMenu ->
            [ text "" |> toUnstyled ]


viewSidebarStatic : Bool -> List (Html msg)
viewSidebarStatic sidebarOpen =
    [ div [ id "sidebar", classList [ ( "open", sidebarOpen ) ], class "static" ]
        [ div [ id "brand" ]
            ([ img [ src "../gingko-leaf-logo.svg", A.width 28 ] [] ]
                ++ (if sidebarOpen then
                        [ h2 [ id "brand-name" ] [ textElmCss En (NoTr "Gingko Writer") ]
                        , div [ id "sidebar-collapse-icon" ] [ AntIcons.leftOutlined [] |> fromUnstyled ]
                        ]

                    else
                        [ text "" ]
                   )
            )
        , ternary sidebarOpen (div [ id "sidebar-document-list-wrap" ] []) (text "")
        , div [ id "new-icon", class "sidebar-button" ] [ AntIcons.fileAddOutlined [] |> fromUnstyled ]
        , div [ id "documents-icon", class "sidebar-button", classList [ ( "open", sidebarOpen ) ] ]
            [ if sidebarOpen then
                AntIcons.folderOpenOutlined [] |> fromUnstyled

              else
                AntIcons.folderOutlined [] |> fromUnstyled
            ]
        , div [ id "document-switcher-icon", class "sidebar-button", class "disabled" ] [ AntIcons.fileSearchOutlined [] |> fromUnstyled ]
        , div
            [ id "help-icon", class "sidebar-button" ]
            [ AntIcons.questionCircleFilled [] |> fromUnstyled ]
        , div [ id "notifications-icon", class "sidebar-button" ] [ AntIcons.bellOutlined [] |> fromUnstyled ]
        , div [ id "account-icon", class "sidebar-button" ] [ AntIcons.userOutlined [] |> fromUnstyled ]
        ]
    ]
        |> List.map toUnstyled
