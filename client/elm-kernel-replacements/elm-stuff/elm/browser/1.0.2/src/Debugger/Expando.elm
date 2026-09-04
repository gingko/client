module Debugger.Expando exposing
  ( Expando
  , Msg
  , init
  , merge
  , update
  , view
  )


import Dict exposing (Dict)
import Elm.Kernel.Debugger
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Json.Decode as Json
import Set exposing (Set)



-- MODEL


type alias Expando =
  { unexpanded : Unexpanded
  , expanded : Set Path
  , viewMore : Dict Path Int
  }


type alias Path = List String


type Unexpanded = Unexpanded


type Expanded
  = S String
  | Primitive String
  | Sequence SeqType (List Unexpanded)
  | Dictionary (List (Unexpanded, Unexpanded))
  | Record (Dict String Unexpanded)
  | Constructor (Maybe String) (List Unexpanded)


type SeqType
  = ListSeq
  | SetSeq
  | ArraySeq


seqTypeToString : Int -> SeqType -> String
seqTypeToString n seqType =
  case seqType of
    ListSeq ->
      "List(" ++ String.fromInt n ++ ")"

    SetSeq ->
      "Set(" ++ String.fromInt n ++ ")"

    ArraySeq ->
      "Array(" ++ String.fromInt n ++ ")"


maximumItemsToView : Path -> Expando -> Int
maximumItemsToView path expando =
  -- Show 100 items at a time.
  (Dict.get path expando.viewMore |> Maybe.withDefault 1) * 100



-- INITIALIZE


init : a -> Expando
init value =
  { unexpanded = Elm.Kernel.Debugger.toUnexpanded value
  , expanded = Set.singleton []
  , viewMore = Dict.empty
  }



-- PRESERVE OLD EXPANDO STATE (open/closed)


merge : a -> Expando -> Expando
merge value expando =
  { expando | unexpanded = Elm.Kernel.Debugger.toUnexpanded value }



-- UPDATE


type Msg
  = Toggle Path
  | ViewMore Path


update : Msg -> Expando -> Expando
update msg expando =
  case msg of
    Toggle path ->
      { expando
        | expanded =
          if Set.member path expando.expanded then
            Set.remove path expando.expanded

          else
            Set.insert path expando.expanded
      }

    ViewMore path ->
      { expando | viewMore = Dict.update path updateViewMoreCount expando.viewMore }


updateViewMoreCount : Maybe Int -> Maybe Int
updateViewMoreCount maybeCount =
  case maybeCount of
    Just count ->
      Just (count + 1)

    Nothing ->
      Just 2



-- VIEW


view : Path -> Expando -> Html Msg
view path expando =
  let
    maybeKey = List.head path
  in
  case Elm.Kernel.Debugger.init expando.unexpanded of
    S stringRep ->
      div (leftPad maybeKey) (lineStarter maybeKey Nothing [ span [ red ] [ text stringRep ] ])

    Primitive stringRep ->
      div (leftPad maybeKey) (lineStarter maybeKey Nothing [ span [ blue ] [ text stringRep ] ])

    Sequence seqType valueList ->
      viewSequence path seqType expando valueList

    Dictionary keyValuePairs ->
      viewDictionary path expando keyValuePairs

    Record valueDict ->
      viewRecord path expando valueDict

    Constructor maybeName valueList ->
      viewConstructor path maybeName expando valueList



-- VIEW SEQUENCE


viewSequence : Path -> SeqType -> Expando -> List Unexpanded -> Html Msg
viewSequence path seqType expando valueList =
  let
    starter = seqTypeToString (List.length valueList) seqType

    maybeKey = List.head path

    isClosed = not (Set.member path expando.expanded)
  in
  div (leftPad maybeKey)
    [ div [ onClick (Toggle path) ] (lineStarter maybeKey (Just isClosed) [ text starter ])
    , if isClosed then text "" else viewSequenceOpen path expando valueList
    ]


viewSequenceOpen : Path -> Expando -> List Unexpanded -> Html Msg
viewSequenceOpen path expando values =
  let
    max = maximumItemsToView path expando
  in
  div [] (viewSequenceOpenHelp path expando 0 max values [])


viewSequenceOpenHelp : Path -> Expando -> Int -> Int -> List Unexpanded -> List (Html Msg) -> List (Html Msg)
viewSequenceOpenHelp path expando index max values acc =
  if index < max then
    case values of
      [] -> List.reverse acc

      value :: rest ->
        viewSequenceOpenHelp
          path
          expando
          (index + 1)
          max
          rest
          (viewConstructorEntry path expando index value :: acc)

  else
    List.reverse (viewMoreButton path :: acc)



-- VIEW DICTIONARY


viewDictionary : Path -> Expando -> List (Unexpanded, Unexpanded) -> Html Msg
viewDictionary path expando keyValuePairs =
  let
    starter = "Dict(" ++ String.fromInt (List.length keyValuePairs) ++ ")"

    maybeKey = List.head path

    isClosed = not (Set.member path expando.expanded)
  in
  div (leftPad maybeKey)
    [ div [ onClick (Toggle path) ] (lineStarter maybeKey (Just isClosed) [ text starter ])
    , if isClosed then text "" else viewDictionaryOpen path expando keyValuePairs
    ]


viewDictionaryOpen : Path -> Expando -> List (Unexpanded, Unexpanded) -> Html Msg
viewDictionaryOpen path expando keyValuePairs =
  let
    max = maximumItemsToView path expando
  in
  div [] (viewDictionaryOpenHelp path expando 0 max keyValuePairs [])


viewDictionaryOpenHelp : Path -> Expando -> Int -> Int -> List (Unexpanded, Unexpanded) -> List (Html Msg) -> List (Html Msg)
viewDictionaryOpenHelp path expando index max keyValuePairs acc =
  if index < max then
    case keyValuePairs of
      [] -> List.reverse acc

      keyValue :: rest ->
        viewDictionaryOpenHelp
          path
          expando
          (index + 1)
          max
          rest
          (viewDictionaryEntry (String.fromInt index :: path) expando keyValue :: acc)

  else
    List.reverse (viewMoreButton path :: acc)


viewDictionaryEntry : Path -> Expando -> (Unexpanded, Unexpanded) -> Html Msg
viewDictionaryEntry path expando ( key, value ) =
  case Elm.Kernel.Debugger.init key of
    S stringRep ->
      view (stringRep :: path) { expando | unexpanded = value }

    Primitive stringRep ->
      view (stringRep :: path) { expando | unexpanded = value }

    _ ->
      div []
        [ view ("key" :: path) { expando | unexpanded = key }
        , view ("value" :: path) { expando | unexpanded = value }
        ]



-- VIEW RECORD


viewRecord : Path -> Expando -> Dict String Unexpanded -> Html Msg
viewRecord path expando record =
  let
    maybeKey = List.head path

    isClosed = not (Set.member path expando.expanded)

    (start, middle, end) =
      if isClosed then
        (Tuple.second (viewTinyRecord record), text "", text "")
      else
        ([ text "{" ], viewRecordOpen path expando record, div (leftPad (Just ())) [ text "}" ])
  in
  div (leftPad maybeKey)
    [ div [ onClick (Toggle path) ] (lineStarter maybeKey (Just isClosed) start)
    , middle
    , end
    ]


viewRecordOpen : Path -> Expando -> Dict String Unexpanded -> Html Msg
viewRecordOpen path expando record =
  div [] (List.map (viewRecordEntry path expando) (Dict.toList record))


viewRecordEntry : Path -> Expando -> ( String, Unexpanded ) -> Html Msg
viewRecordEntry path expando ( field, value ) =
  view (field :: path) { expando | unexpanded = value }



-- VIEW CONSTRUCTOR


viewConstructor : Path -> Maybe String -> Expando -> List Unexpanded -> Html Msg
viewConstructor path maybeName expando valueList =
  let
    maybeKey = List.head path

    isClosed = not (Set.member path expando.expanded)

    tinyArgs = List.map (Tuple.second << viewExtraTiny) valueList

    description =
      case (maybeName, tinyArgs) of
        (Nothing  , []     ) -> [ text "()" ]
        (Nothing  , x :: xs) -> text "( " :: span [] x :: List.foldr (\args rest -> text ", " :: span [] args :: rest) [ text " )" ] xs
        (Just name, []     ) -> [ text name ]
        (Just name, x :: xs) -> text (name ++ " ") :: span [] x :: List.foldr (\args rest -> text " " :: span [] args :: rest) [] xs

    (maybeIsClosed, openHtml) =
        case valueList of
          [] ->
            (Nothing, div [] [])

          [ entry ] ->
            case Elm.Kernel.Debugger.init entry of
              S _ ->
                (Nothing, div [] [])

              Primitive _ ->
                (Nothing, div [] [])

              Sequence _ subValueList ->
                ( Just isClosed
                , if isClosed then div [] [] else
                    viewSequenceOpen ("0" :: path) expando subValueList
                )

              Dictionary keyValuePairs ->
                ( Just isClosed
                , if isClosed then div [] [] else
                    viewDictionaryOpen ("0" :: path) expando keyValuePairs
                )

              Record record ->
                  ( Just isClosed
                  , if isClosed then div [] [] else
                      viewRecordOpen ("0" :: path) expando record
                  )

              Constructor _ subValueList ->
                  ( Just isClosed
                  , if isClosed then div [] [] else
                      viewConstructorOpen ("0" :: path) expando subValueList
                  )

          _ ->
            ( Just isClosed
            , if isClosed then div [] [] else viewConstructorOpen ("0" :: path) expando valueList
            )
  in
  div (leftPad maybeKey)
    [ div [ onClick (Toggle path) ] (lineStarter maybeKey maybeIsClosed description)
    , openHtml
    ]


viewConstructorOpen : Path -> Expando -> List Unexpanded -> Html Msg
viewConstructorOpen path expando valueList =
  div [] (List.indexedMap (viewConstructorEntry path expando) valueList)


viewConstructorEntry : Path -> Expando -> Int -> Unexpanded -> Html Msg
viewConstructorEntry path expando index value =
  view (String.fromInt index :: path) { expando | unexpanded = value }



-- VIEW TINY


viewTiny : Unexpanded -> ( Int, List (Html msg) )
viewTiny value =
  case Elm.Kernel.Debugger.init value of
    S stringRep ->
      let
        str = elideMiddle stringRep
      in
      ( String.length str
      , [ span [ red ] [ text str ] ]
      )

    Primitive stringRep ->
      ( String.length stringRep
      , [ span [ blue ] [ text stringRep ] ]
      )

    Sequence seqType valueList ->
      viewTinyHelp <| seqTypeToString (List.length valueList) seqType

    Dictionary keyValuePairs ->
      viewTinyHelp <| "Dict(" ++ String.fromInt (List.length keyValuePairs) ++ ")"

    Record record ->
      viewTinyRecord record

    Constructor maybeName [] ->
      viewTinyHelp <| Maybe.withDefault "Unit" maybeName

    Constructor maybeName valueList ->
      viewTinyHelp <|
        case maybeName of
          Nothing -> "Tuple(" ++ String.fromInt (List.length valueList) ++ ")"
          Just name -> name ++ " …"


viewTinyHelp : String -> ( Int, List (Html msg) )
viewTinyHelp str =
  (String.length str, [ text str ])


elideMiddle : String -> String
elideMiddle str =
  if String.length str <= 18
  then str
  else String.left 8 str ++ "..." ++ String.right 8 str



-- VIEW TINY RECORDS


viewTinyRecord : Dict String Unexpanded -> ( Int, List (Html msg) )
viewTinyRecord record =
  if Dict.isEmpty record then
    (2, [ text "{}" ])
  else
    viewTinyRecordHelp 0 "{ " (Dict.toList record)


viewTinyRecordHelp : Int -> String -> List ( String, Unexpanded ) -> ( Int, List (Html msg) )
viewTinyRecordHelp length starter entries =
  case entries of
    [] ->
        (length + 2, [ text " }" ])

    (field, value) :: rest ->
      let
        fieldLen = String.length field
        (valueLen, valueHtmls) = viewExtraTiny value
        newLength = length + fieldLen + valueLen + 5
      in
      if newLength > 60 then
        (length + 4, [ text ", … }" ])
      else
        let
          (finalLength, otherHtmls) = viewTinyRecordHelp newLength ", " rest
        in
        ( finalLength
        , text starter
            :: span [ purple ] [ text field ]
            :: text " = "
            :: span [] valueHtmls
            :: otherHtmls
        )


viewExtraTiny : Unexpanded -> ( Int, List (Html msg) )
viewExtraTiny value =
  case Elm.Kernel.Debugger.init value of
    Record record ->
      viewExtraTinyRecord 0 "{" (Dict.keys record)

    _ ->
      viewTiny value


viewExtraTinyRecord : Int -> String -> List String -> ( Int, List (Html msg) )
viewExtraTinyRecord length starter entries =
  case entries of
    [] ->
      (length + 1, [ text "}" ])

    field :: rest ->
      let
        nextLength = length + String.length field + 1
      in
      if nextLength > 18 then
        (length + 2, [ text "…}" ])

      else
        let
          (finalLength, otherHtmls) = viewExtraTinyRecord nextLength "," rest
        in
        ( finalLength
        , text starter :: span [ purple ] [ text field ] :: otherHtmls
        )



-- VIEW HELPERS


viewMoreButton : Path -> Html Msg
viewMoreButton path =
  div (leftPad (List.head path))
    [ div (onClick (ViewMore path) :: leftPad (Just ())) [text ("View more")]
    ]


lineStarter : Maybe String -> Maybe Bool -> List (Html msg) -> List (Html msg)
lineStarter maybeKey maybeIsClosed description =
  let
    arrow =
      case maybeIsClosed of
        Nothing    -> makeArrow ""
        Just True  -> makeArrow "▸"
        Just False -> makeArrow "▾"
  in
  case maybeKey of
    Nothing ->
      arrow :: description

    Just key ->
      arrow :: span [ purple ] [ text key ] :: text " = " :: description


makeArrow : String -> Html msg
makeArrow arrow =
  span
    [ style "color" "#777"
    , style "padding-left" "2ch"
    , style "width" "2ch"
    , style "display" "inline-block"
    ]
    [ text arrow ]


leftPad : Maybe a -> List (Html.Attribute msg)
leftPad maybeKey =
  case maybeKey of
    Nothing -> []
    Just _  -> [ style "padding-left" "4ch" ]


red : Html.Attribute msg
red =
  style "color" "rgb(196, 26, 22)"


blue : Html.Attribute msg
blue =
  style "color" "rgb(28, 0, 207)"


purple : Html.Attribute msg
purple =
  style "color" "rgb(136, 19, 145)"
