module Doc.Metadata exposing (Metadata, decoder, decoderImport, encode, getCreatedAt, getDocId, getDocName, getUpdatedAt, isSameDocId, listDecoder, new, rename)

import Coders exposing (maybeToValue)
import Json.Decode as Dec exposing (Decoder)
import Json.Decode.Extra exposing (datetime)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Enc
import RandomId exposing (fromObjectId)
import Time


type Metadata
    = Metadata String MetadataRecord


type alias MetadataRecord =
    { docName : Maybe String
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    , rev : Maybe String
    }


new : String -> Metadata
new docId =
    Metadata docId
        { docName = Nothing
        , rev = Nothing
        , createdAt = Time.millisToPosix 0
        , updatedAt = Time.millisToPosix 0
        }


getDocId : Metadata -> String
getDocId (Metadata docId _) =
    docId


getDocName : Metadata -> Maybe String
getDocName (Metadata _ { docName }) =
    docName


getCreatedAt : Metadata -> Time.Posix
getCreatedAt (Metadata _ { createdAt }) =
    createdAt


getUpdatedAt : Metadata -> Time.Posix
getUpdatedAt (Metadata _ { updatedAt }) =
    updatedAt


isSameDocId : Metadata -> Metadata -> Bool
isSameDocId m1 m2 =
    getDocId m1 == getDocId m2



-- JSON


decoder : Decoder Metadata
decoder =
    Dec.map5 (\id n c u r -> Metadata id (MetadataRecord n c u r))
        (Dec.field "docId" Dec.string)
        (Dec.field "name" (Dec.maybe Dec.string))
        (Dec.field "createdAt" Dec.int |> Dec.map Time.millisToPosix)
        (Dec.field "updatedAt" Dec.int |> Dec.map Time.millisToPosix)
        (Dec.field "_rev" (Dec.maybe Dec.string))


listDecoder : Decoder (List Metadata)
listDecoder =
    Dec.field "rows" (Dec.list (Dec.field "value" decoder))


decoderImport : Decoder (Maybe Metadata)
decoderImport =
    let
        builder id n c u d =
            if d then
                Nothing

            else
                Just <|
                    Metadata (fromObjectId id) (MetadataRecord n c u Nothing)
    in
    Dec.succeed builder
        |> required "_id" Dec.string
        |> required "name" (Dec.maybe Dec.string)
        |> required "createdAt" datetime
        |> required "updatedAt" datetime
        |> optional "deleted" Dec.bool False


encode : Metadata -> Enc.Value
encode (Metadata docId { docName, createdAt, updatedAt, rev }) =
    case rev of
        Just revData ->
            Enc.object
                [ ( "_id", Enc.string "metadata" )
                , ( "docId", Enc.string docId )
                , ( "name", maybeToValue Enc.string docName )
                , ( "createdAt", Enc.int (Time.posixToMillis createdAt) )
                , ( "updatedAt", Enc.int (Time.posixToMillis updatedAt) )
                , ( "_rev", Enc.string revData )
                ]

        Nothing ->
            Enc.object
                [ ( "_id", Enc.string "metadata" )
                , ( "docId", Enc.string docId )
                , ( "name", maybeToValue Enc.string docName )
                , ( "createdAt", Enc.int (Time.posixToMillis createdAt) )
                , ( "updatedAt", Enc.int (Time.posixToMillis updatedAt) )
                ]


rename : String -> Metadata -> Enc.Value
rename newDocName (Metadata docId record) =
    Metadata docId
        { record
            | docName = Just newDocName
        }
        |> List.singleton
        |> List.map encode
        |> Enc.list identity
