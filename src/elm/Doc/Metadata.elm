module Doc.Metadata exposing (Metadata, decoder, encode, getDocId, getDocName, new, rename, setRev)

import Coders exposing (maybeToValue)
import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc


type Metadata
    = Metadata
        { docId : String
        , docName : Maybe String
        , rev : Maybe String
        }


new : String -> Metadata
new docId =
    Metadata { docId = docId, docName = Nothing, rev = Nothing }


getDocId : Metadata -> String
getDocId (Metadata { docId }) =
    docId


getDocName : Metadata -> Maybe String
getDocName (Metadata { docName }) =
    docName


setRev : String -> Metadata -> Metadata
setRev newRev (Metadata oldMetadata) =
    Metadata { oldMetadata | rev = Just newRev }



-- JSON


decoder : Decoder Metadata
decoder =
    Dec.map3 (\i n r -> Metadata { docId = i, docName = n, rev = r })
        (Dec.field "docId" Dec.string)
        (Dec.field "name" (Dec.maybe Dec.string))
        (Dec.field "_rev" (Dec.maybe Dec.string))


encode : Metadata -> Dec.Value
encode (Metadata { docId, docName, rev }) =
    case rev of
        Just revData ->
            Enc.object
                [ ( "_id", Enc.string "metadata" )
                , ( "docId", Enc.string docId )
                , ( "name", maybeToValue Enc.string docName )
                , ( "_rev", Enc.string revData )
                ]

        Nothing ->
            Enc.object
                [ ( "_id", Enc.string "metadata" )
                , ( "docId", Enc.string docId )
                , ( "name", maybeToValue Enc.string docName )
                ]


rename : String -> Metadata -> Dec.Value
rename newDocName (Metadata { docId, docName, rev }) =
    Metadata
        { docId = docId
        , docName = Just newDocName
        , rev = rev
        }
        |> encode
