port module Import.Incoming exposing (importComplete)

-- SUBSCRIPTIONS


port importComplete : (Maybe String -> msg) -> Sub msg
