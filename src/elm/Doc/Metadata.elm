module Doc.Metadata exposing (Metadata, decoder, encode, getDocName, new)

import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc


type Metadata
    = Metadata
        { docName : Maybe String
        , rev : Maybe String
        }


new : Metadata
new =
    Metadata { docName = Nothing, rev = Nothing }


getDocName : Metadata -> Maybe String
getDocName (Metadata { docName }) =
    docName



-- JSON


decoder : Decoder Metadata
decoder =
    Dec.map2 (\n r -> Metadata { docName = n, rev = r })
        (Dec.field "name" (Dec.maybe Dec.string))
        (Dec.field "_rev" (Dec.maybe Dec.string))


encode : String -> Metadata -> Dec.Value
encode newDocName (Metadata { rev }) =
    case rev of
        Just revData ->
            Enc.object
                [ ( "_id", Enc.string "metadata" )
                , ( "name", Enc.string newDocName )
                , ( "_rev", Enc.string revData )
                ]

        Nothing ->
            Enc.object
                [ ( "_id", Enc.string "metadata" )
                , ( "name", Enc.string newDocName )
                ]
