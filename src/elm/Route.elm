module Route exposing (Route(..), pushUrl, replaceUrl, toString)

import Browser.Navigation as Nav
import Import.Template as Template exposing (Template)
import Url.Builder as Builder


type Route
    = Root
    | Signup
    | Login
    | ForgotPassword (Maybe String)
    | ResetPassword String
    | EmailConfirmed
    | DocNew
    | DocUntitled String
    | Doc String String
    | Copy String
    | Import Template
    | Upgrade Bool


toString : Route -> String
toString route =
    case route of
        Root ->
            "/"

        Signup ->
            "/signup"

        Login ->
            "/login"

        ForgotPassword email_ ->
            case email_ of
                Just email ->
                    "/forgot-password" ++ ([ Builder.string "email" email ] |> Builder.toQuery)

                Nothing ->
                    "/forgot-password"

        ResetPassword token ->
            "/reset-password/" ++ token

        EmailConfirmed ->
            "/confirm/"

        DocNew ->
            "/new"

        DocUntitled dbName ->
            "/" ++ dbName

        Doc dbName docName ->
            "/" ++ dbName ++ "/" ++ docName

        Copy dbName ->
            "/copy/" ++ dbName

        Import template ->
            "/import/" ++ Template.toString template

        Upgrade isOk ->
            "/upgrade/"
                ++ (if isOk then
                        "success"

                    else
                        "cancelled"
                   )


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl navKey route =
    Nav.replaceUrl navKey (toString route)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl navKey route =
    Nav.pushUrl navKey (toString route)
