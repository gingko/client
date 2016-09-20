module Utils exposing (..)

import List.Extra as ListExtra

type alias PosetEntry =
  { id: String
  , prev: Maybe String
  , next: Maybe String
  }


type alias DagEntry =
  { id: String
  , prev: List String
  , next: List String
  }


toDag : List PosetEntry -> List DagEntry
toDag posets =
  let
    getPrev : PosetEntry -> List String
    getPrev pos =
      let
        prevId =
          case pos.prev of
            Nothing -> []
            Just pid -> [pid]
      in
        posets
          |> List.filter (\p -> p.next == Just pos.id) -- List PosetEntry
          |> List.map .id -- List String
          |> List.append prevId
          |> ListExtra.unique -- List String

    getNext : PosetEntry -> List String
    getNext pos =
      let
        nextId =
          case pos.next of
            Nothing -> []
            Just pid -> [pid]
      in
        posets
          |> List.filter (\p -> p.prev == Just pos.id) -- List PosetEntry
          |> List.map .id -- List String
          |> List.append nextId
          |> ListExtra.unique -- List String

    f : PosetEntry -> DagEntry
    f p = DagEntry p.id (getPrev p) (getNext p)
  in
    posets -- List { id: String, prev: Maybe String, next: Maybe String }
      |> List.map f


sortFunction : List DagEntry -> DagEntry -> DagEntry -> Order
sortFunction dag a b =
  let
    distA = (maxDist dag "end" a.id)
    distB = (maxDist dag "end" b.id)
  in
    if distA == distB then
      compare a.id b.id
    else
      compare distB distA


testDag : List DagEntry
testDag =
  [ DagEntry "start" [] ["1"]
  , DagEntry "1" ["start"] ["2","end"]
  , DagEntry "2" ["1"] ["3", "end"]
  , DagEntry "3" ["2"] ["4", "end"]
  , DagEntry "4" ["3"] ["end"]
  , DagEntry "end" ["1","2","3","4"] []
  ]


testPoset : List PosetEntry
testPoset =
  [ PosetEntry "start" Nothing (Just "end")
  , PosetEntry "end" (Just "start") Nothing
  , PosetEntry "a" (Just "start") (Just "end") 
  , PosetEntry "b" (Just "a") (Just "end") 
  , PosetEntry "c" (Just "b") (Just "end") 
  , PosetEntry "y" (Just "b") (Just "c") 
  , PosetEntry "z" (Just "b") (Just "c") 
  , PosetEntry "x" (Just "a") (Just "b") 
  , PosetEntry "f" (Just "x") (Just "y") 
  ]


maxDist : List DagEntry -> String -> String -> Int
maxDist dag toId fromId =
  if toId == fromId then
    0
  else
    let
      maxNexts =
        dag -- List DagEntry
          |> ListExtra.find (\de -> de.id == fromId) -- Maybe DagEntry
          |> Maybe.withDefault (DagEntry "end" ["start"] []) -- DagEntry
          |> .next
          |> List.map (maxDist dag toId)
          |> List.maximum
          |> Maybe.withDefault 0
    in
      1 + maxNexts


linearizeDag : List DagEntry -> List String
linearizeDag dag =
  dag -- List DagEntry
    |> List.sortWith (sortFunction dag) -- List DagEntry
    |> List.map .id
