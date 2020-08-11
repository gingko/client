module Doc.Metadata exposing (Metadata, decoder)

import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc


type alias Metadata =
    { docName : Maybe String
    , rev : Maybe String
    }



-- JSON


decoder : Decoder Metadata
decoder =
    Dec.map2 Metadata
        (Dec.field "name" (Dec.maybe Dec.string))
        (Dec.field "_rev" (Dec.maybe Dec.string))
