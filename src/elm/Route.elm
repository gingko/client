module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = Home
    | Login
    | Doc String String
    | DocUntitled String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Doc (string </> string)
        , Parser.map DocUntitled string
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url
