module Route exposing (Route(..), fromUrl, pushUrl, replaceUrl, toString)

import Browser.Navigation as Nav
import Import.Template as Template exposing (Template)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s, string)
import Url.Parser.Query as Query


type Route
    = Home
    | Signup
    | Login
    | ForgotPassword (Maybe String)
    | ResetPassword String
    | DocNew
    | DocUntitled String
    | Doc String String
    | Import Template


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Signup (s "signup")
        , Parser.map Login (s "login")
        , Parser.map ForgotPassword (s "forgot-password" <?> Query.string "email")
        , Parser.map ResetPassword (s "reset-password" </> string)
        , Parser.map Import (s "import" </> Parser.custom "IMPORT" Template.fromString)
        , Parser.map DocNew (s "new")
        , Parser.map DocUntitled string
        , Parser.map Doc (string </> string)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


toString : Route -> String
toString route =
    case route of
        Home ->
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

        DocNew ->
            "/new"

        DocUntitled dbName ->
            "/" ++ dbName

        Doc dbName docName ->
            "/" ++ dbName ++ "/" ++ docName

        Import template ->
            "/import/" ++ Template.toString template


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl navKey route =
    Nav.replaceUrl navKey (toString route)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl navKey route =
    Nav.pushUrl navKey (toString route)
