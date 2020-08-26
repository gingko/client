module Route exposing (Route(..), fromUrl, pushUrl, replaceUrl)

import Browser.Navigation as Nav
import Dict
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s, string)
import Url.Parser.Query as Q


type Route
    = Home
    | Signup
    | Login
    | DocNew String
    | DocUntitled String
    | Doc String String


parser : Parser (Route -> a) a
parser =
    let
        queryNew =
            Q.enum "new" (Dict.fromList [ ( "true", True ) ])

        newOrUntitled docId new_ =
            case new_ of
                Just True ->
                    DocNew docId

                _ ->
                    DocUntitled docId
    in
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Signup (s "signup")
        , Parser.map Login (s "login")
        , Parser.map newOrUntitled (string <?> queryNew)
        , Parser.map Doc (string </> string)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


routeToString : Route -> String
routeToString route =
    case route of
        Home ->
            "/"

        Signup ->
            "/signup"

        Login ->
            "/login"

        DocNew dbName ->
            "/" ++ dbName ++ "?new=true"

        DocUntitled dbName ->
            "/" ++ dbName

        Doc dbName docName ->
            "/" ++ dbName ++ "/" ++ docName


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl navKey route =
    Nav.replaceUrl navKey (routeToString route)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl navKey route =
    Nav.pushUrl navKey (routeToString route)
